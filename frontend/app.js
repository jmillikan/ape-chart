var appGuide = angular.module('app-guide', ['ngRoute']);

appGuide.config(['$locationProvider', '$routeProvider', ($locationProvider, $routeProvider) => {
    $locationProvider.hashPrefix('!');

    $routeProvider
        .when('/', { templateUrl: 'partials/front.html' })
        .when('/:appId', { templateUrl: 'partials/app.html' });
}]);

appGuide.controller('AppController', ['$rootScope', 'state', ($rootScope, state) => {
    $rootScope.networkState = state.networkState;
}]);

appGuide.controller('ChooseAppController', ['$scope', 'state', ($scope, state) => {
    $scope.apps = [];

    state.getJwt("jmillikan", "jmillikan").then((jwtData) => {
	console.log("JWT (good):");
	console.log(jwtData);
    });

    state.getJwt("gingie", "bottle").then((jwtData) => {
	console.log("JWT (bad):");
	console.log(jwtData);
    });

    $scope.refreshApps = () => state.getApps().then((apps) => $scope.apps = apps);

    // Directive as t->inf
    $scope.newApp = {
        visible: false,
        show: () => { $scope.newApp.visible = true; },
        hide: () => { $scope.newApp.visible = false; },
        name: '', // Form, 2-way
        description: '', // Form, 2-way
        add: () => state.addApp($scope.newApp.name, $scope.newApp.description).then(s => {
            $scope.refreshApps();
            $scope.newApp.hide();
        })
    };

    $scope.refreshApps();
}]);

/* 
   Relationship of $scope with controllers through here is lumpy.
   Thankfully everything used is either immediate (UI states), from 1 level up (stateId), or in fudge scope (addRootState etc).
*/
appGuide.controller('AppFudgeController', ['$scope', 'state', '$routeParams', '$location', ($scope, state, $routeParams, $location) => {
    $scope.appId = Number($routeParams.appId);
    $scope.app = null;
    $scope.confirmDelete = () => {
	if(confirm('Really delete entire app "' + ($scope.app.name || '(serious error: Missing app name)') + '"?')){
	    state.deleteApp($scope.appId).then(r => $location.url('/'));
	}
    };
    
    state.getApp($scope.appId).then(app => $scope.app = app);

    $scope.states = []; // $scope.states is also used creating commands.
    
    $scope.refreshAppStates = () =>
	state.getStates($scope.appId).then((states) => $scope.states = states);
    $scope.refreshAppStates();

    $scope.processes = [];
    $scope.refreshProcesses = () =>
	state.getProcesses($scope.appId).then((processes) => $scope.processes = processes);
    $scope.refreshProcesses();

    $scope.processId = null;

    $scope.newProcess = {
	visible: false,
	name: '',
	description: '',
	show: () => $scope.newProcess.visible = true,
	hide: () => {
	    $scope.newProcess.visible = false;
	    $scope.newProcess.name = '';
	    $scope.newProcess.description = '';
	},
	create: () =>
	    state.addProcess($scope.appId, $scope.newProcess.name, $scope.newProcess.description)
	    .then((newProcess) => {
		$scope.newProcess.hide();
		$scope.refreshProcesses();
		//$scope.processId = TODO...
	    })
    };

    // Show out-of-process
    $scope.oop = false;
}]);

appGuide.controller('MultiTreeController', ['$scope', 'state', ($scope, state) => {
    console.log('Loading process selection/multi state tree');
    
    $scope.appStateRootIds = [];
    $scope.addRootState = (id) => {
        id = Number(id); // This is sloppy but works for now.
	$scope.refreshAppStates();
        
        // Note: The filter at the end of state.html also tries to prevent this wrt the state id stack.
        // Need to get explicit about handling the whole thing at some point.

        // Check explicitly, but this will error on dupes because of ngRepeat.
        if(!$scope.appStateRootIds.includes(id)) $scope.appStateRootIds.push(id);
        else console.log("Attempt to add duplicate top-level state (Not supported).");
    };

    $scope.removeRootState = (rid) => {
        rid = Number(rid);

        $scope.appStateRootIds = $scope.appStateRootIds.filter((id) => id != rid);
    };
    
    $scope.newState = {
        name: '', description: '',
        visible: false,
        show: () => $scope.newState.visible = true,
        hide: () => $scope.newState.visible = false,
        add: () => {	    
            state.addState($scope.appId, $scope.newState.name, $scope.newState.description)
		.then($scope.addRootState);
            $scope.newState.hide();
        }
    }
}]);

appGuide.controller('StateTreeController', ['$scope', ($scope) => {
    console.log('Loading tree for ' + $scope.rootStateId);

    $scope.stateId = $scope.rootStateId;
    // We could track the stack explicitly through $scope.$parent chain, 
    // but that's a hassle and probably counterproductive
    $scope.stateIdStack = [$scope.stateId];
}]);

appGuide.controller('StateController', ['$scope', 'state', ($scope, state) => {
    console.log('Loading state ' + $scope.stateId);

    $scope.state = null;

    $scope.refreshState = () => 
        state.getStateDetails($scope.stateId).then((s) => $scope.state = s);

    $scope.refreshState();
    
    $scope.addCommand = false;

    $scope.moveToRoot = () => {
        $scope.addRootState($scope.stateId);
        if($scope.toggleExpanded) $scope.toggleExpanded();
    };

    $scope.includes = {
        stateId: null,
        addState: () => {
            state.addIncludeState($scope.stateId, $scope.includes.stateId).then(() => $scope.refreshState());
        },
        removeState: (includeStateId) => {
            state.removeIncludeState($scope.stateId, includeStateId).then(() => $scope.refreshState());
        }
    };

    // To prevent "included commands" with a different stateId
    // from opening the included state, the stateId of those commands is put on the stack.
    // This is done in CommandController, but could be done here by collecting
    // additional stateIds from scope.commands.

    // This confusion calls for a rethinking of where I'm managing $scope.stateId.
}]);

appGuide.controller('CommandController', ['$scope', 'state', ($scope, state) => {
    console.log('Loading command ' + $scope.command.id);

    // Name for clarity.
    var resultState = $scope.command.resultStateId; 
    // stateId will be needed by the next nested state/StateController if any.
    $scope.stateId = resultState;

    $scope.stateIdStack = $scope.stateIdStack.slice();

    // Command may come from an "include state"
    // This is a hack to prevent the first ill side-effect, which is 
    //included commands opening when they probably shouldn't
    // In the other case, this is just a dup of the last one on the stack.
    $scope.stateIdStack.push($scope.command.stateId); 

    // THEN we check for result state...
    // does the state stack contain the state this command points to?
    $scope.stateInStack = $scope.stateIdStack.includes(resultState);

    // THEN push the result state
    $scope.stateIdStack.push(resultState);
    
    // UI toggle
    $scope.resultStateExpanded = false;

    $scope.toggleExpanded = () => $scope.resultStateExpanded = !$scope.resultStateExpanded;
    $scope.expandResultState = () => $scope.resultStateExpanded = true;
    $scope.hideResultState = () => $scope.resultStateExpanded = false;

    $scope.removeCommand = () => {
        state.removeCommand($scope.command.id, $scope.processId).then((r) => $scope.refreshState());
    };

    $scope.deleteCommand = () => {
        state.deleteCommand($scope.command.id).then((r) => $scope.refreshState());
    };
}]);

appGuide.controller('AddCommandController', ['$scope', 'state', ($scope, state) => {
    $scope.clearCommand = () =>
        $scope.c = {
            methodType: 'keyboard-emacs',
            method: '',
            desc: '',
            note: '',
            resultStateId: '',

            stateName: '',
            stateDesc: ''
        };
    
    $scope.createCommand = () =>
        state.addCommand($scope.stateId, $scope.processId, $scope.c).then(c => {
            $scope.refreshState(); // Swiped from state
	    $scope.refreshAppStates(); // Might have added a state
            $scope.hideAddCommand();

	    $scope.clearCommand();
        });

    $scope.clearCommand();

    $scope.addCommandVisible = false;
    $scope.showAddCommand = () => $scope.addCommandVisible = true;
    $scope.hideAddCommand = () => $scope.addCommandVisible = false;
}]);

appGuide.factory('state', ['$http', '$timeout', '$rootScope', '$q', ($http, $timeout, $rootScope, $q) => {
    var networkState = {networkTrouble: false};
    // Very rough handling of this, just to provide something...

    var initialDelay = 200; // ms
    var troubleThreshold = 3200; // ms
    var quitThreshold = 10000; // ms

    // Exponential backoff returning a new promise.
    var qBackoff = (p) => {
	var newP = $q.defer(); // ATGTI rewrite to get rid of $q.defer...
	
        var delay = initialDelay; // ms

        // Thunk - continue then increase delay
        var c = () => {
            p().then(r => {
                networkState.networkTrouble = false;

		newP.resolve(r);
            }, ex => {
		delay = delay * 2;

		if(delay >= troubleThreshold) 
                    networkState.networkTrouble = true;

		if(delay >= quitThreshold){
		    newP.reject("Gave up retrying network operation after " + quitThreshold / 1000 + "s");

		    console.log("Rejected request due to threshold");

		    return;
		}

		console.log("Delayed request due to failure");

                $timeout(c, delay); // timeout starts AFTER failure
            });
        }

        // Begin backoff with initialDelay
        c();

	return newP.promise;
    };

    var httpData = p => p.then(r => r.data);

    return {
	networkState: networkState,
	getJwt: (username, password) => httpData($http.post('/jwt', {username, password})),
        getApps: (callback) =>
	    httpData(qBackoff(() => $http.get('/app'))),
        getApp: (appId) =>
	    httpData(qBackoff(() => $http.get('/app/' + appId))),
	deleteApp: (appId) =>
	    httpData($http.delete('/app/' + appId)),
        addApp: (name, description) =>
	    httpData($http.post('/app', {name: name, description: description}, {})),
        getStates: (appId, callback) =>
            httpData(qBackoff(() => $http.get('/app/' + appId + '/state/'))),
        getProcesses: (appId) => 
            httpData(qBackoff(() => $http.get('/app/' + appId + '/process'))),
	addProcess: (appId, name, description) =>
	    httpData($http.post('/app/' + appId + '/process', {name, description})),
        addState: (appId, name, description) =>
            httpData($http.post('/state', {name, description, appId}, {})),
        getStateDetails: (stateId, callback) =>
	    httpData(qBackoff(() => $http.get('/state/' + stateId))),
        addCommand: (stateId, processId, command) => 
            httpData($http.post('/state/' + stateId + '/process/' + (processId ? processId : -1) + '/command', 
				command, {})),
        addIncludeState: (stateId, includeStateId) =>
            httpData($http.post('/state/' + stateId + '/include_state/' + includeStateId)),
        removeIncludeState: (stateId, includeStateId) => 
            httpData($http.delete('/state/' + stateId + '/include_state/' + includeStateId)),
        deleteCommand: (commandId) => 
            httpData($http.delete('/command/' + commandId)),
        removeCommand: (commandId, processId) => 
            httpData($http.delete('/command/' + commandId + '/process/' + processId))
    };
}]);

var inProcess = (c,pid) => c.process.some((p) => p.processId == pid);

appGuide.filter('inProcess', () => inProcess);

appGuide.filter('forProcess', () => (input, pid, outOfProcess) => {
    // input is e.g. [{ /* command */ id: '1', method: 'C-c', process: [{ /* command process */ processId: '1'}]}]
    // Order by primary process

    if(!input){
        return input;
    }

    console.log("outOfProcess: " + outOfProcess);
    var commands = outOfProcess || !pid ? input : input.filter(c => inProcess(c, pid));

    return commands.slice().sort((a, b) => {
        if(inProcess(a, pid) && inProcess(b, pid)) return a.method.localeCompare(b.method);
        if(inProcess(a, pid)) return -1;
        if(inProcess(b, pid)) return 1;
        return 0;
    });
});

// https://stackoverflow.com/questions/11442632/how-can-i-post-data-as-form-data-instead-of-a-request-payload
appGuide.config(['$httpProvider', function($httpProvider) {
    // Intercept POST requests, convert to standard form encoding
    $httpProvider.defaults.headers.post["Content-Type"] = "application/x-www-form-urlencoded";
    $httpProvider.defaults.transformRequest.unshift(function (data, headersGetter) {
        var key, result = [];

        if (typeof data === "string")
            return data;

        for (key in data) {
            if (data.hasOwnProperty(key))
                result.push(encodeURIComponent(key) + "=" + encodeURIComponent(data[key]));
        }
        return result.join("&");
    });
}]);
