-- Pseudocode mixed with Haskell on the graph ML idea

-- basic data types
-- Object User
data User = User {name::String, age:: Int, role:: Role}
-- Object Post
data Post = Post {title::String, body:: Text}
-- Object 'writes' that is an Arrow between User and Post. 'require = right' means 'writes' is mandatory for Posts
data Arrow = User -writes{when::Date | require = right }-> Post

-- or define all relations between records as function declarations,
-- based on which the system would generate how to store
-- 1:many, "nothing" allowed
rel writes :: User -> Maybe [Post]
-- 1:1, mandatory
rel author :: Post -> User


User 1->0.. Post
User 1->0.. Comment

-- permissions
-- allow any action on a User if it is initiated by the Admin
allow action User = True | action -source-> user is Admin
-- allow any action on a User if it is initiated from the Trusted Node (e.g., server)
						 | action -source-> node is Trusted

 -- allow creation of the Post only if it is initiated by the logged in User
allow create Post = True | create -source-> user is LoggedIn
-- allow update / delete on the post only by the Author
allow [action <- | update, delete ] Post p = True | (action -source-> user -writes-> p) is True

-- Startup on the Server (Master Node)
-- initial load of the data object graph
Graph mainGraph = loadgraph mainGraph "last 50 posts"
-- making mainGraph available on the other nodes
publish mainGraph

-- Client (Web Client Node)
-- making sure clientGraph is synchronized (according to permissions!) with mainGraph (whatever is published)
Graph clientGraph <-synchronize-> (GVM -> mainNode -> mainGraph)

{-
TODO: basic setup -
- authenticate a user:
- login
- logout

TODO: GUI part, let's start with the web
- view list of posts
	- view / edit a post
	- delete a post
- create new post
- view all posts for a specific user
-}

-- authentication
-- get credentials from somewhere, negotiate with the server node, authenticate a user on the given node =>
-- need remote method calls or some such
credentials >> authenticate

-- remote function call:
authenticate (GVM -> mainNode) credentials >> processResult
-- LOOOOTS of questions here: if async, (>> processResult), how do we handle call backs? if sync - everything freezes... Errors?
-- so if async: write as if it's synced. then it gets processed and all reactivities fire up correspondingly. so, REACTIVITY is key.

-- OK, let's try to detail authentication process:
-- GVM - contains ALL nodes information, synchronizes them between server nodes for redundancy (ping server nodes, if down - reconnect clients or something)
-- login: ______ password: ________ [submit] = widgetLogin

-- reactive behavior dependnence defined
widgetLoginBehavior = show wxLoggedIn : wxLoggedOut ? GVM -> currentUser is LoggedIn
-- html
show wxLoggedIn = "<p> Welcome, {#name}! </p>"
-- REDUNDANT! we are already tied in the widgetLoginBehavior!
wxLoggedIn -bind-> GVM -> currentUser

-- html: needs to be redone to support templates etc
show wxLoggedOut = "<p>login: {login}<br>password: {password}<br><button data-action='login'/>"
-- declarative statement that authenticate should process events from wxLoggedOut;
-- showResult observes result of authenticate in turn - so, sync or async is irrelevant
showResult -observe-> authenticate -observe-> wxLoggedOut
-- what the action does
authenticate (login, password) = authenticate (GVM -> mainNode) login (encrypt password)
