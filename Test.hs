module Test where

import SloLog.Database
import SloLog.Unify
import SloLog.Query

db = [
    (Compound [Atom "foo", Atom "bar"], Pass),
    (Compound [Atom "foo", Atom "baz"], Pass),
    (Compound [Atom "quux", Atom "bar"], Pass),
    (Compound [Atom "same", Variable "x", Variable "x"], Pass),
    (Compound [Atom "friends", Variable "x", Variable "y"],
        Conjunct [
            Query $ Compound [Variable "x", Atom "bar"],
            Query $ Compound [Variable "y", Atom "bar"],
            Negation $ Query (Compound [Atom "same", Variable "x", Variable "y"]) 
            ])
    ]
   
query = Conjunct [
    Query (Compound [Variable "x", Atom "bar"]),
    Query (Compound [Variable "x", Atom "baz"])
    ]

query2 = Disjunct [
    Query (Compound [Variable "x", Atom "bar"]),
    Query (Compound [Variable "x", Atom "baz"])
    ]

query3 = Conjunct [
    Query (Compound [Variable "x", Atom "bar"]),
    Query (Compound [Variable "y", Atom "baz"]),
    Negation $ Query (Compound [Atom "same", Variable "x", Variable "y"])
    ]

graph = [
    (Compound [Atom "link", Atom "a", Atom "b"], Pass),
    (Compound [Atom "link", Atom "b", Atom "c"], Pass),
    (Compound [Atom "link", Atom "b", Atom "e"], Pass),
    (Compound [Atom "link", Atom "c", Atom "d"], Pass),
    (Compound [Atom "path", Variable "x", Variable "x"], Pass),
    (Compound [Atom "path", Variable "x", Variable "y"],
        Conjunct [
            Query $ Compound [Atom "link", Variable "x", Variable "intermed"],
            Query $ Compound [Atom "path", Variable "intermed", Variable "y"]
            ])
    ]

path = Query $ Compound [Atom "path", Atom "a", Variable "x"]

