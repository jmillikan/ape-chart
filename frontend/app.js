var appGuide = angular.module('app-guide', []);

appGuide.controller('FudgeController', ['$scope', ($scope) => {
    console.log('Loading fudge.');

    $scope.stateId = 1;
    $scope.processId = 1;
    $scope.appId = 1;
}]);

appGuide.controller('StateController', ['$scope', 'state', ($scope, state) => {
    console.log('Loading state ' + $scope.stateId);

    $scope.refreshState = () => 
        state.getProcessState($scope.stateId, $scope.processId, 
                              (s) => $scope.state = s);

    $scope.refreshState();
}]);

appGuide.controller('CommandController', ['$scope', ($scope) => {
    // stateId will be needed by the next nested state/StateController if any.
    $scope.stateId = $scope.command.resultStateId;
    
    // These two need to be in scope. 
    $scope.noExpand = false;
    // Not used right now.
    $scope.expandResultState = false; // Very important to put this in THIS scope...
}]);

appGuide.controller('AddCommandController', ['$scope', 'state', ($scope, state) => {
    $scope.clearCommand = () => {
        $scope.c = {
            methodType: 'keyboard-emacs',
            method: '',
            desc: '',
            note: '',
            resultStateId: '',
            stateName: '',
            stateDesc: ''
        };
    }

    state.getStates($scope.appId, states => $scope.states = states);

    $scope.createCommand = () => {
        state.addCommand($scope.stateId, $scope.processId, $scope.c, () => {
            $scope.refreshState(); // Swiped from state
            $scope.clearCommand();
        });
    };

    $scope.clearCommand();
}]);

appGuide.factory('state', ['$http', ($http) => {
    return {
        getProcessState(stateId, processId, callback){
            $http.get('/state/' + stateId + '/process/' + processId)
                .then(response => callback(response.data), 
                      response => console.log('Failed to fetch state ' + stateId + ' in process ' + processId));
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
