> {-# LANGUAGE UnicodeSyntax #-}
> module Database
> where
> import Unicode

> type Person  =  (Name, Age, FavouriteCourse)
>
> type Name             =  String
> type Age              =  Integer
> type FavouriteCourse  =  String

> frits, peter, ralf ∷ Person
> frits  =  ("Frits",  33,  "Algorithms and Data Structures")
> peter  =  ("Peter",  57,  "Imperative Programming")
> ralf   =  ("Ralf",   33,  "Functional Programming")
> luka   =  ("Luka", 23, "Semantics and Pragmatics")

> students   ∷  [Person]
> students   =  [frits, peter, ralf, luka]

> age ∷ Person → Age
> age (_n, a, _c)  =  a

1. Add your own data and/or invent some additional entries. In par-
ticular, add yourself to the list of students.

2. Define functions
  name :: Person → Name
  favouriteCourse :: Person → FavouriteCourse
that extract name and favourite course, respectively.

> name             ∷ Person → Name
> name (n, _a, _c ) = n

> favouriteCourse  ∷ Person → FavouriteCourse
> favouriteCourse (_n, _a, c) = c

3. Define a function showPerson::Person → String that returns a string
representation of a person.

> showPerson       ∷ Person → String
> showPerson (n, a, c) = let join s1 s2 = s1 ++ ", " ++ s2
>                        in join n (join (show a) c)

4. Define a function twins :: Person → Person → Bool that checks
whether two persons are twins. (For lack of data, we agree that
two persons are twins if they are of the same age.)

> twins            ∷ Person → Person → Bool
> twins p1 p2 = (age p1) == (age p2)

5. Define a function increaseAge :: Person → Person which increases the age of
a given person by one e.g.

> increaseAge      ∷ Person → Person
> increaseAge (n, a, c) = (n, a + 1, c)

6. Create expressions to solve the following tasks:
a) increment the age of all students by two;

> agedUp = map increaseAge students

b) promote all of the students (attach "dr " to their name);

> promoted = let promote (n, a, c) = ("dr " ++ n, a, c)
>            in map promote students

c) find all students named Frits;

> fritses = filter (\p → (name p) == "Frits") students

d) find all students whose favourite course is Functional Programming;

> fpfans = filter (\p → (favouriteCourse p) == "Functional Programming") students

e) find all students who are in their twenties;

> millenials = filter (\p → ((age p) ≥ 20) ∧ ((age p) < 30)) students

f) find all students whose favourite course is Functional Programming and who are in their twenties;

> fp_fans_and_millenials = let twenties = \p → ((age p) ≥ 20) ∧ ((age p) < 30)
>                              fpFan = \p → (favouriteCourse p) == "Functional Programming"
>                          in filter (\p → (twenties p) ∧ (fpFan p)) students

g) find all students whose favourite course is Imperative Programming or who are in their twenties.

> fp_fans_and_millenials = let twenties = \p → ((age p) ≥ 20) ∧ ((age p) < 30)
>                              ipFan = \p → (favouriteCourse p) == "Imperative Programming"
>                          in filter (\p → (twenties p) ∧ (fpFan p)) students
