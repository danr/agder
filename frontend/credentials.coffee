agder_module.factory 'credentials', ($http, make_url) ->

    hash = () -> CryptoJS.SHA256(salt + password).toString(CryptoJS.enc.Hex)

    update_credentials = () ->
        if status == "CredentialsOK"
            Credentials:
                cred_user: username
                cred_hash: hash()
        else
            Anonymous: []

    username = ""
    password = ""
    salt = ""
    status = "NotLoggedIn"
    credentials = update_credentials()

    logout = (cb) ->
        username = ""
        password = ""
        salt = ""
        status = "NotLoggedIn"
        credentials = update_credentials()
        cb()

    logout ->

    parse_status = (res) ->
        for key of res
            return key
        return "NotLoggedIn"

    get_salt = (cont) ->
        $http.post(make_url("/salt"), d: username)
            .error(console.log)
            .success (res) ->
                salt = res
                status = "SaltReceived"
                cont()

    register = (cb, _username, _password) ->
        username = _username
        password = _password
        get_salt () ->
            $http.post(make_url("/register"),
                Credentials:
                    cred_user: username
                    cred_hash: hash()
            ).success (res) ->
                status = parse_status res
                cb()
                if status == "SuccessfulCreation"
                    login(cb, _username, _password)

    login = (cb, _username, _password) ->
        username = _username
        password = _password
        get_salt () ->
            $http.post(make_url("/login"),
                Credentials:
                    cred_user: username
                    cred_hash: hash()
            ).success (res) ->
                status = parse_status res
                update_credentials()
                cb()

    get: () -> credentials
    parse_status: parse_status
    status: () -> status
    register: register
    login: login
    logout: logout


