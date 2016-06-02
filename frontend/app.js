var appGuide = angular.module('app-guide', ['ngRoute']);

appGuide.config(['$locationProvider', '$routeProvider', ($locationProvider, $routeProvider) => {
    $locationProvider.hashPrefix('!');

    $routeProvider.
        when('/', { templateUrl: 'partials/app.html' });

        // when('/phones', {
        //     template: '<phone-list></phone-list>'
        // }).
        // when('/phones/:phoneId', {
        //     template: '<phone-detail></phone-detail>'
        // }).
        // otherwise('/phones');
}]);

/* 
   Relationship of $scope with controllers through here is lumpy.
   Thankfully everything used is either immediate (UI states), from 1 level up (stateId), or in fudge scope (addRootState etc).
*/
appGuide.controller('AppFudgeController', ['$scope', 'state', ($scope, state) => {
    console.log('Loading app fudge');

    $scope.appId = 1;

    $scope.states = []; // $scope.states is also used creating commands.
    state.getStates($scope.appId, (states) => $scope.states = states);

    $scope.processes = [];
    state.getProcesses($scope.appId, (processes) => $scope.processes = processes);

    $scope.processId = null;

    // Show out-of-process
    $scope.oop = false;
}]);

appGuide.controller('MultiTreeController', ['$scope', 'state', ($scope, state) => {
    console.log('Loading process selection/multi state tree');
    
    $scope.appStateRootIds = [];
    $scope.addRootState = (id) => {
        id = Number(id); // This is sloppy but works for now.
        
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
            // TODO: Failure callback
            state.addState($scope.appId, $scope.newState.name, $scope.newState.description, 
                           $scope.addRootState);
            $scope.addStateVisible = false;
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
        state.getStateDetails($scope.stateId, (s) => $scope.state = s);

    $scope.refreshState();
    
    $scope.addCommand = false;

    $scope.moveToRoot = () => {
        $scope.addRootState($scope.stateId);
        if($scope.toggleExpanded) $scope.toggleExpanded();
    };

    $scope.includes = {
        stateId: null,
        addState: () => {
            state.addIncludeState($scope.stateId, $scope.includes.stateId, () => $scope.refreshState());
        },
        removeState: (includeStateId) => {
            state.removeIncludeState($scope.stateId, includeStateId, () => $scope.refreshState());
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
        state.removeCommand($scope.command.id, $scope.processId, (r) => $scope.refreshState());
    };

    $scope.deleteCommand = () => {
        state.deleteCommand($scope.command.id, (r) => $scope.refreshState());
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
        state.addCommand($scope.stateId, $scope.processId, $scope.c, () => {
            $scope.refreshState(); // Swiped from state
            $scope.hideAddCommand();
        });

    $scope.clearCommand();

    $scope.addCommandVisible = false;
    $scope.showAddCommand = () => $scope.addCommandVisible = true;
    $scope.hideAddCommand = () => $scope.addCommandVisible = false;
}]);

appGuide.factory('state', ['$http', ($http) => {
    return {
        addState(appId, name, description, callback){
            $http.post('/state', {name: name, description: description, appId: appId}, {})
                .then(response => callback(response.data),
                      response => console.log('Failed to add state'));
        },
        getStateDetails(stateId, callback){
            $http.get('/state/' + stateId)
                .then(response => callback(response.data), 
                      response => console.log('Failed to fetch state ' + stateId));
        },
        addCommand(stateId, processId, command, callback){
            $http.post('/state/' + stateId + '/process/' + processId + '/command', 
                       command, {})
                .then(response => callback(), 
                      response => console.log('Failed to post new command'));
        },
        getStates(appId, callback){
            $http.get('/app/' + appId + '/state/')
                .then(response => callback(response.data), 
                      response => console.log('Failed to fetch state list for app ' + appId));
        },
        addIncludeState(stateId, includeStateId, callback){
            $http.post('/state/' + stateId + '/include_state/' + includeStateId)
                .then(response => callback(response.data),
                      response => console.log('Failed to add include state'));
        },
        removeIncludeState(stateId, includeStateId, callback){
            $http.delete('/state/' + stateId + '/include_state/' + includeStateId)
                .then(response => callback(response.data),
                      response => console.log('Failed to add include state'));
        },
        deleteCommand(commandId, callback){
            $http.delete('/command/' + commandId)
                .then(response => callback(response.data),
                      response => console.log('Failed to delete command'));
        },
        removeCommand(commandId, processId, callback){
            $http.delete('/command/' + commandId + '/process/' + processId)
                .then(response => callback(response.data),
                      response => console.log('Failed to remove command-process'));
        },
        getProcesses(appId, callback){
            $http.get('/app/' + appId + '/process')
                .then(response => callback(response.data),
                      response => console.log('Failed to fetch processes for app ' + appId));
        }
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
        if(inProcess(a, pid) && inProcess(b, pid)) return 0;
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
