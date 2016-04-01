test1 <- "POST /auth/github HTTP/1.1
User-Agent: MyClient/1.0.0
Accept: application/vnd.travis-ci.2+json
Host: api.travis-ci.org
Content-Type: application/json
Content-Length: 37

{\"github_token\":\"YOUR GITHUB TOKEN\"}"

test2 <- "GET /users HTTP/1.1
User-Agent: MyClient/1.0.0
Accept: application/vnd.travis-ci.2+json
Host: api.travis-ci.org
Authorization: token \"YOUR TRAVIS ACCESS TOKEN\""

test3 <- "GET / HTTP/1.1
User-Agent: MyClient/1.0.0
Accept: application/vnd.travis-ci.2+json
Host: api.travis-ci.org"

test4 <- "GET /?acl=something HTTP/1.1
Host: BucketName.s3.amazonaws.com
Date: date"

