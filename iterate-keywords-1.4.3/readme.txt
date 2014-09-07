What is this?
-------------

Iterate-keywords is a patched version of iterate 
(http://common-lisp.net/project/iterate/)

where you can use keywords for clause heads. 
E.g.
(iter (:for i from 1 to 3) (:collect i))

You can _not_ use usual words as clause heads anymore, 
so 
(iter (for i .... ) ... ) won't work.
Current version of iterate-keywords is based on iterate 1.4.3 

Stability
---------
Test suite runned successfully under sbcl-1-0.22. 
Iterate-keywords loads and seem to work under 
lispworks-6.1 personal for windows. Considered beta.


Bugs
----
Report bugs to comp.lang.lisp to a budden

