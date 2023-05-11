# cl-ghost

This is an API wrapper for the [Ghost CDN](https://ghost.org)
Its a pretty basic wrapper that doesn't perform query parameter checks or content type checks.
It relies on Dex for posting content and doesn't auto encode JSON data or Multipart form.
There is a slot in the `endpoint` object called `body` that you set correctly.
Look at the docs for [Dexador](https://github.com/fukamachi/dexador) To see how to encode multipart-form.

For information about correct query parameters see the [Ghost docs](https://ghost.org/docs/)

The `key` query parameter is automatically applied for Content API requests and the JWT is
generated automatically from your `admin-api-key`. 

## Getting started

There is a global variable called *ghost*. First you must bind this to an instance of either `ghost-admin` or just `ghost`.

```lisp
(setf *ghost* (make-instance 'ghost
                             :content-api-key "my-content-api-key"
                             :api-url "https://<my-ghost-instance>/ghost/api/content"))
```
For admin
```lisp
(setf *ghost* (make-instance 'admin:ghost-admin
                             :content-api-key "my-content-api-key"
                             :api-url "https://<my-ghost-instance>/ghost/api/content"
                             :admin-url "https://<my-ghost-instance>/ghost/api/admin"
                             :admin-api-key "my-admin-api-key"))
```
You have to set both `api-url` and `admin-url` because the admin part of Ghost can be hosted
separately ([apparently](https://ghost.org/docs/admin-api/)).

When using admin you need to login

```lisp
(login *ghost* 30 :minutes)
```
That will create a JWT that wont expire for 30 minutes.
When you attempt to make requests using `#'call-api` You may have to handle a condition
called `expired-jwt` there is a restart called `renew` that will automatically renew
the jwt based on 2 dynamic variables `*amount*` and `*unit*`
see: the :before method for `#'call-api` in  `src/admin-api.lisp` for more info.

Once that is done to make a request simply do the following:

```lisp
(call-api (make-instance 'content:posts-browse))
```
To add query parameters:
```lisp
(call-api (make-instance 'content:posts-browse :query '(("include" . "authors"))))
```

There are different query parameters available for different endpoints so make sure you checkout the documentation.


## License

MIT

