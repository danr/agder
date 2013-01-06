window.agder_module = angular.module('agder', [])

agder_module.factory 'make_url', () -> (s) -> "http://#{window.location.host}" + s
