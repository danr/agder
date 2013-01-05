make_url = (s) -> "http://#{window.location.host}" + s

angular.module('agder', [])

    .directive('markdown', () ->
        # http://blog.angularjs.org/2012/05/custom-components-part-1.html
        # http://jsfiddle.net/8bENp/66/
        converter = new Showdown.converter()
        link = (scope, element, attrs, model) ->
            render = () ->
                htmlText = converter.makeHtml model.$modelValue
                element.html(htmlText)

            scope.$watch attrs['ngModel'], render
            render()
        restrict: 'E'
        require: 'ngModel'
        link: link

    ).controller('ProblemsCtrl', ($scope,$http) ->
        $scope.problems = []

        $http.get(make_url "/problems")
            .error(console.log)
            .success (res) -> $scope.problems = res

    ).controller('ProblemCtrl', ($scope,$http,$location) ->
        # The identifier of this problem
        $id = $scope.id

        angular.extend $scope,
            # These are set in updateShow below
            problem:  ""
            definitions: ""
            description: ""
            #The result of a problem
            result: ""
            solved: false

        # Problem showed or not is in $scope.show
        # This is in the location bar
        $scope.loc = $location

        $scope.$watch 'loc.search()', () ->
            $scope.show = $location.search()[$id]?
            $scope.updateShow()

        $scope.$watch 'show', () ->
            $location.search($id, $scope.show or null)

        $scope.toggleShow = () ->
            $scope.show = !$scope.show
            $scope.updateShow()

        $scope.updateShow = () ->
            if $scope.show and not $scope.problem
                $http.get(make_url "/problem/#{$id}")
                    .error(console.log)
                    .success (res) ->
                        $scope.problem = res.problem
                        $scope.definitions = res.definitions or ""

        # Solution attempt
        $scope.submit = () ->
            $scope.result = "Submitted!"
            $http.post(make_url("/solve/#{$id}"), $scope.problem)
                .error(console.log)
                .success (res) ->
                    $scope.result = res.stdout.replace ////home/dan/code/agder/solutions/#{$id}/[^/]*/([^\.]*).agda///g, (filename, short) ->
                        short + ".agda"

                    if res.exitcode == 0
                        $scope.solved = true
    )

