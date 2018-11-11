/*
   proposition as types
   total typing (no any)
   undefined with a better name
   primitive types
   type first design
   dead code ellimination
   named params
   curry automatically
   missing match
   the hole technic
   product type
   basic variants (coproduct, sum type)
   pattern match
   pattern match (nested)
   refmt
 */

/*
  Poster post a task.
    Poster Post Task
 */

/* tagged type vs type alias, constructor */
type userId =
  | UserId(string);

type userAttributesShape = {id: userId};

/* constructor on shape to futher constrait the weakness of shape */
type userAttributes =
  | UserAttr(userAttributesShape);

/* multiple tagged shape */
type user =
  | Admin(userAttributes)
  | Poster(userAttributes)
  | Tasker(userAttributes);

/* again provide tag/constructor for a primitive type, which is too flexible */
type taskName =
  | TaskName(string);

type taskAttributes = {
  name: taskName,
  userId,
};

type task =
  | Task(taskAttributes);

let post = (~user, ~taskName: string) =>
  switch (user) {
  | Admin(UserAttr({id}))
  | Poster(UserAttr({id})) => Some(Task({name: TaskName(taskName), userId: id}))
  | _ => None
  };

let poster = Poster(UserAttr({id: UserId("123")}));
let postWithUser = post(~user=poster);

let job = postWithUser(~taskName="cleaning");
