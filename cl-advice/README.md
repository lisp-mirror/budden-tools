## cl-advice - an attempt of portable define-advice macro

Advice facility is present in at least CCL, SBCL, Allegro and LW. 
In CCL it is rather limited, in SBCL it is unexported. We use some implementation-dependent internal symbols
to provide a portability layer. Main symbols are: 

## API

#### `[Macro]` cl-advice:define-advice 
`function-name-no-eval decorator-fn-eval &key (advice-name #| no eval |# default)`

Defines an advice. In SBCL, definition is findable with find-source command.

#### `[Function]` cl-advice:uninstall-advice

Removes an advice dynamically.

#### `[Function]` cl-advice:install-advice 
`function-name decorator-fn &key (advice-name 'default)`

Adds an advice dynamically. Avoid using it. 


### See also
See also `:cl-advice` [package definition](package.lisp)

### Rules
- Advice can only be defined for existing function
- There are only "around" advices
- Advice is a function which receives first argument - a next advice (or original function), and all arguments that original function received. 
- In SBCL and CCL, one function can have several advices with different names; in other implementations, the only advice with the name `cl-advice::default` can exist 
- Many functions can have different advices with the same name (FIXME test it)
- New advice with some name replaces the old one with the same name
- When the function is redefined with defun, things must keep working in SBCL and CCL, but may break in other implementations (FIXME test it)
- If there are several advices for one fn, order of their nesting is undefined

### Example
See [tests](cl-advice-tests.lisp)

### Implementation
We patch both CCL and SBCL dynamically. It used to work in SBCL 1.4.2 and CCL 1.12 dev. Tests are executed when the system is loaded. Advice facility in CCL is modified, and local function is introduced to call next advice. This may affect performance of other ccl:advise users or cause warnings.  

### Plans
- Close all FIXMEs
- Add definition source location for CCL. 

### Non-plans
- Before and after advices
- Testing of other implementations
- Using native advise facilities in LW and ACL
- Multiple advices for other implementations

All those things from non-plans should be rather easy, but I have no need to implement them. Contributors are welcome.

### For CCL and SBCL support teams
Accepting my patches to CCL and SBCL and any feedback are welcome, you know how to reach me. 

### License
(C) Denis Budyak 2015-2017, MIT License
