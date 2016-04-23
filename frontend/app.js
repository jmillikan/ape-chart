var appGuide = angular.module('app-guide', []);

appGuide.controller('FudgeController', ['$scope', function($scope){
    console.log('Loading fudge.');

    $scope.stateId = 1;
    $scope.processId = 1;
    $scope.stateStack = [1];
}]);

appGuide.controller('StateController', ['$scope', 'state', function($scope, state){
   console.log('Loading state ' + $scope.stateId);

    state($scope.stateId, $scope.processId, function(s){
        $scope.state = s;
        HELLO_STATE = s;
    });
}]);

appGuide.controller('CommandController', ['$scope', function($scope){
    $scope.stateId = $scope.command.resultStateId;
    $scope.noExpand = false;
    $scope.expandResultState = false; // Very important to put this in THIS scope...
}]);

appGuide.factory('state', ['$http', function($http){
    return function(stateId, processId, callback){
        $http.get('/state/' + stateId + '/process/' + processId)
            .then(function(response){
                callback(response.data);
            }, function(response){
                console.log('Failed to fetch state ' + stateId + ' in process ' + processId);
            });
    };
}]);
