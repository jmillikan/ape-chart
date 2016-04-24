var appGuide = angular.module('app-guide', []);

appGuide.controller('FudgeController', ['$scope', function($scope){
    console.log('Loading fudge.');

    $scope.stateId = 1;
    $scope.processId = 1;
    $scope.appId = 1;
    $scope.stateStack = [1];
}]);

appGuide.controller('StateController', ['$scope', 'state', function($scope, state){
    console.log('Loading state ' + $scope.stateId);

    $scope.refreshState = function(){
        state.getProcessState($scope.stateId, $scope.processId, function(s){
            $scope.state = s;
            HELLO_STATE = s;
        });
    }

    $scope.refreshState();
}]);

appGuide.controller('CommandController', ['$scope', function($scope){
    // stateId will be needed by the next nested state/StateController if any.
    $scope.stateId = $scope.command.resultStateId;
    
    // These two need to be in scope. 
    $scope.noExpand = false;
    // Not used right now.
    $scope.expandResultState = false; // Very important to put this in THIS scope...
}]);

appGuide.controller('AddCommandController', ['$scope', 'state', function($scope, state){
    $scope.clearCommand = function(){
        $scope.c = {
            methodType: 'keyboard-emacs',
            method: '',
            desc: '',
            resultStateId: '',
            note: ''
        };
    }

    state.getStates($scope.appId, function(states){
        $scope.states = states;
    });

    $scope.createCommand = function(){
        state.addCommand($scope.stateId, $scope.processId, $scope.c, function(){
            $scope.refreshState(); // Swiped from state
            $scope.clearCommand();
        });
    };

    $scope.clearCommand();
}]);

appGuide.factory('state', ['$http', function($http){
    return {
        getProcessState: function(stateId, processId, callback){
            $http.get('/state/' + stateId + '/process/' + processId)
                .then(function(response){
                    callback(response.data);
                }, function(response){
                    console.log('Failed to fetch state ' + stateId + ' in process ' + processId);
                });
        },
        addCommand: function(stateId, processId, command, callback){
            $http.post('/state/' + stateId + '/process/' + processId + '/command', 
                       command, {})
                .then(function(response){
                    callback();
                }, function(reponse){
                    console.log('Failed to post new command');
                });
        },
        getStates: function(appId, callback){
            $http.get('/app/' + appId + '/state/')
                .then(function(response){
                    callback(response.data);
                }, function(response){
                    console.log('Failed to fetch state list for app ' + appId);
                });
        }};
}]);

// https://stackoverflow.com/questions/11442632/how-can-i-post-data-as-form-data-instead-of-a-request-payload
appGuide.config(['$httpProvider', function ($httpProvider) {
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
