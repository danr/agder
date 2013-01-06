agder_module.controller 'LoginCtrl', ($scope,credentials) ->

    $scope.username = ""
    $scope.password = ""
    $scope.status = "NotLoggedIn"

    $scope.logged_in = () -> $scope.status == "CredentialsOK"

    $scope.execute = (fn) -> () ->
        cb = -> console.log $scope.status = credentials.status()
        fn cb, $scope.username, $scope.password

    $scope.register = $scope.execute(credentials.register)
    $scope.login = $scope.execute(credentials.login)
    $scope.logout = ->
        $scope.username = ""
        $scope.password = ""
        $scope.execute(credentials.logout)()

