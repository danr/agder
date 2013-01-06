// Generated by CoffeeScript 1.4.0
(function() {

  agder_module.controller('LoginCtrl', function($scope, credentials) {
    $scope.username = "";
    $scope.password = "";
    $scope.status = "NotLoggedIn";
    $scope.logged_in = function() {
      return $scope.status === "CredentialsOK";
    };
    $scope.execute = function(fn) {
      return function() {
        var cb;
        cb = function() {
          return console.log($scope.status = credentials.status());
        };
        return fn(cb, $scope.username, $scope.password);
      };
    };
    $scope.register = $scope.execute(credentials.register);
    $scope.login = $scope.execute(credentials.login);
    return $scope.logout = function() {
      $scope.username = "";
      $scope.password = "";
      return $scope.execute(credentials.logout)();
    };
  });

}).call(this);
