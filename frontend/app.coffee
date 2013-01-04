make_url = (s) -> "http://#{window.location.host}" + s

window.ProblemsCtrl = ($scope,$http) ->
    $scope.problems = []

    promise = $http
        method: "GET"
        url: make_url "/problems"

    promise.success (res) ->
        $scope.problems = res

    promise.error (res) ->
        console.log "Error: ", res

window.ProblemCtrl = ($scope,$http) ->
    $id = $scope.id
    $scope.problem = ""
    $scope.solved = false

    $scope.show = false
    $scope.toggleShow = () ->
        $scope.show = !$scope.show
        if $scope.show and not $scope.problem
            promise = $http
                method: "GET"
                url: make_url "/problem/#{$id}"

            promise.success (res) ->
                $scope.problem = res

            promise.error (res) ->
                console.log "Error: ", res

    if $id == "DeMorgan"
        $scope.toggleShow()

    $scope.result = ""

    $scope.submit = () ->
        $scope.result = "Submitted!"
        submit_promise = $http
            method: "POST"
            url: make_url "/solve/#{$id}"
            data: $scope.problem

        submit_promise.success (res) ->
            $scope.result = res.stdout.replace ////home/dan/code/agder/solutions/#{$id}/[^/]*/([^\.]*).agda///g, (filename, short) ->
                short + ".agda"

            if res.exitcode == 0
                $scope.solved = true

        submit_promise.error (res) ->
            $scope.result = res

