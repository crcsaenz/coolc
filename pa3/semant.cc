

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr)
{
    inheritanceMap = new std::map<Symbol, Symbol>();
    classEnv = new std::map<Symbol, class__class>();
    methodEnv = new std::map<Symbol, std::map<Symbol, method_class> >();
    attrEnv = new std::map<Symbol, std::map<Symbol, attr_class> >();
    // 1. Look at all classes and build the inheritance graph and class table
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
    {
        class__class *c = (class__class *)classes->nth(i);
        Symbol cName = c->getName();
        Symbol cParent = c->getParent();
        // check for base class re-definition
        if (cName == Object || cName == Int || cName == Bool || cName == Str || cName == IO)
            semant_error(c) << "Redefinition of basic class " << cName->get_string() << endl;
        // check for duplicate class definition)
        else if (inheritanceMap->count(cName) != 0)
            semant_error(c) << "Class " << cName->get_string() << " was previously defined" << endl;
        else {
            inheritanceMap->insert(std::pair<Symbol, Symbol>(cName, cParent));
            classEnv->insert(std::pair<Symbol, class__class>(cName, *c));
        }
    }
    // check for Main class
    if (inheritanceMap->count(Main) == 0) {
        semant_error() << "Class Main is not defined" << endl;
    }
    // add base classes to graph and class table
    install_basic_classes();
}

void ClassTable::install_basic_classes() {
    
    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.
    
    //
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.
    
    Class_ Object_class =
	class_(Object,
	       No_class,
	       append_Features(
                           append_Features(
                                           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                                           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
                           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);
    inheritanceMap->insert(std::pair<Symbol, Symbol>(Object, No_class));
    classEnv->insert(std::pair<Symbol, class__class>(Object, *(class__class *)Object_class));
    
    //
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class =
	class_(IO,
	       Object,
	       append_Features(
                           append_Features(
                                           append_Features(
                                                           single_Features(method(out_string, single_Formals(formal(arg, Str)),
                                                                                  SELF_TYPE, no_expr())),
                                                           single_Features(method(out_int, single_Formals(formal(arg, Int)),
                                                                                  SELF_TYPE, no_expr()))),
                                           single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
                           single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);
    inheritanceMap->insert(std::pair<Symbol, Symbol>(IO, Object));
    classEnv->insert(std::pair<Symbol, class__class>(IO, *(class__class *)IO_class));
    
    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer.
    //
    Class_ Int_class =
	class_(Int,
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);
    inheritanceMap->insert(std::pair<Symbol, Symbol>(Int, Object));
    classEnv->insert(std::pair<Symbol, class__class>(Int, *(class__class *)Int_class));
    
    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);
    inheritanceMap->insert(std::pair<Symbol, Symbol>(Bool, Object));
    classEnv->insert(std::pair<Symbol, class__class>(Bool, *(class__class *)Bool_class));
    
    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //
    Class_ Str_class =
	class_(Str,
	       Object,
	       append_Features(
                           append_Features(
                                           append_Features(
                                                           append_Features(
                                                                           single_Features(attr(val, Int, no_expr())),
                                                                           single_Features(attr(str_field, prim_slot, no_expr()))),
                                                           single_Features(method(length, nil_Formals(), Int, no_expr()))),
                                           single_Features(method(concat, 
                                                                  single_Formals(formal(arg, Str)),
                                                                  Str, 
                                                                  no_expr()))),
                           single_Features(method(substr, 
                                                  append_Formals(single_Formals(formal(arg, Int)), 
                                                                 single_Formals(formal(arg2, Int))),
                                                  Str, 
                                                  no_expr()))),
	       filename);
    inheritanceMap->insert(std::pair<Symbol, Symbol>(Str, Object));
    classEnv->insert(std::pair<Symbol, class__class>(Str, *(class__class *)Str_class));
}

bool program_class::inheritanceCheck(ClassTable *classTable)
{
    bool wellFormed = true;
    // check inheritance of each class separately
    std::map<Symbol, Symbol>::iterator it;
    for (it = classTable->inheritanceMap->begin(); it != classTable->inheritanceMap->end(); ++it)
    {
        Symbol child = (Symbol)it->first;
        Symbol parent = (Symbol)it->second;
        // walk up inheritance path to check for cycles or errors
        std::set<Symbol> seen;
        seen.insert(child);
        while (true)
        {
            // if is Object, or inherits from Object, done and on to the next one
            if (child == Object || parent == Object)
                break;
            // check for inheritance from base class other than Object or IO
            if (parent == Int || parent == Str || parent == Bool) {
                classTable->semant_error(&classTable->classEnv->find(child)->second) << "Class " << child->get_string() << " cannot inherit class " << parent->get_string() << endl;
                wellFormed = false;
                break;
            }
            // check for existance of inherited class
            if (classTable->inheritanceMap->count(parent) == 0) {
                classTable->semant_error(&classTable->classEnv->find(child)->second) << "Class " << child->get_string() << " inherits from an undefined class " << parent->get_string() << endl;
                wellFormed = false;
                break;
            }
            // repeated class in path = cycle
            if (seen.count(parent) != 0) {
                classTable->semant_error(&classTable->classEnv->find(child)->second) << "Class " << parent->get_string() << ", or an ancestor of " << parent->get_string() << ", is involved in an inheritance cycle" << endl;
                wellFormed = false;
                break;
            }
            child = parent;
            seen.insert(child);
            parent = classTable->inheritanceMap->find(parent)->second;
        }
    }
    return wellFormed;
}

/////////////////////////////////////
// Declaration-gathering functions //
/////////////////////////////////////
void program_class::gatherDeclarations(ClassTable *classTable)
{
    // first pass to populate method/attr maps
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        class__class *c = (class__class *)classes->nth(i);
        c->gatherDeclarations(classTable);
    }
    // second pass to check for inherited method/attr errors
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        class__class *c = (class__class *)classes->nth(i);
        c->checkDeclarations(classTable);
    }
}

void class__class::gatherDeclarations(ClassTable *classTable)
{
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature_class *f = features->nth(i);
        f->gatherDeclarations(classTable, this);
    }
}
void class__class::checkDeclarations(ClassTable *classTable)
{
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature_class *f = features->nth(i);
        f->checkDeclarations(classTable, this);
    }
}

void method_class::gatherDeclarations(ClassTable *classTable, class__class *c)
{
    std::map<Symbol, method_class> methods;
    // retrieve current methods for current class if available
    if (classTable->methodEnv->count(c->getName()))
        methods = classTable->methodEnv->find(c->getName())->second;
    // report error if method is redefined
    if (methods.count(name))
        classTable->semant_error(c) << "Method " << name->get_string() << " is multiply defined" << endl;
    // otherwise, insert the new method
    else
        methods.insert(std::pair<Symbol, method_class>(name, *this));
    // update method environment
    classTable->methodEnv->insert(std::pair<Symbol, std::map<Symbol, method_class> >(c->getName(), methods));
}
void method_class::checkDeclarations(ClassTable *classTable, class__class *c)
{
    // walk up the inheritance path of the class c, checking for redefinition
    // of an inherited method
    
    // if inherited method redefined, check for same formals and return type
    // otherwise, error
}

void attr_class::gatherDeclarations(ClassTable *classTable, class__class *c)
{
    std::map<Symbol, attr_class> attributes;
    // retrieve current attributes for current class if available
    if (classTable->attrEnv->count(c->getName()))
        attributes = classTable->attrEnv->find(c->getName())->second;
    // report error if attribute is redefined
    if (attributes.count(name) != 0)
        classTable->semant_error(c) << "Attribute " << name->get_string() << " is multiply defined" << endl;
    // otherwise insert the new attribute
    else
        attributes.insert(std::pair<Symbol, attr_class>(name, *this));
    // update attribute environment
    classTable->attrEnv->insert(std::pair<Symbol, std::map<Symbol, attr_class> >(c->getName(), attributes));
}
void attr_class::checkDeclarations(ClassTable *classTable, class__class *c)
{
    // walk up the inheritance path of the class c, checking for redefinition
    // of an inherited attribute
    
    // if inherited attribute, error
}

/////////////////////////////
// Type-checking functions //
/////////////////////////////
void program_class::typecheck(ClassTable *classT,
                              SymbolTable<Symbol, method_class> *methods,
                              SymbolTable<Symbol, Symbol> *objects)
{
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        class__class *c = (class__class *)classes->nth(i);
        c->typecheck(classT, methods, objects);
    }
}

void class__class::typecheck(ClassTable *classT,
                             SymbolTable<Symbol, method_class> *methods,
                             SymbolTable<Symbol, Symbol> *objects)
{
    for (int i = features->first(); features->more(i); i = features->next(i)) {
        Feature_class *f = (Feature_class *)features->nth(i);
        f->typecheck(classT, methods, objects);
    }
}

void method_class::typecheck(ClassTable *classT,
                             SymbolTable<Symbol, method_class> *methods,
                             SymbolTable<Symbol, Symbol> *objects)
{
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        formal_class *f = (formal_class *)formals->nth(i);
        f->typecheck(classT, methods, objects);
    }
    expr->typecheck(classT, methods, objects);
}

void attr_class::typecheck(ClassTable *classT,
                           SymbolTable<Symbol, method_class> *methods,
                           SymbolTable<Symbol, Symbol> *objects)
{
    init->typecheck(classT, methods, objects);
}

void formal_class::typecheck(ClassTable *classT,
                             SymbolTable<Symbol, method_class> *methods,
                             SymbolTable<Symbol, Symbol> *objects)
{
    // check declared types are all actual types
}

Symbol branch_class::typecheck(ClassTable *classT,
                             SymbolTable<Symbol, method_class> *methods,
                             SymbolTable<Symbol, Symbol> *objects)
{
    return expr->typecheck(classT, methods, objects);
}

Symbol assign_class::typecheck(ClassTable *classT,
                             SymbolTable<Symbol, method_class> *methods,
                             SymbolTable<Symbol, Symbol> *objects)
{
    Symbol s = expr->typecheck(classT, methods, objects);
    set_type(s);
    return s;
}

Symbol static_dispatch_class::typecheck(ClassTable *classT,
                                      SymbolTable<Symbol, method_class> *methods,
                                      SymbolTable<Symbol, Symbol> *objects)
{
    expr->typecheck(classT, methods, objects);
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        Expression_class *e = actual->nth(i);
        e->typecheck(classT, methods, objects);
    }
    // return should be return type of method looked up
    // in methods table - which I haven't yet populated :(
    set_type(Object);
    return Object;
}

Symbol dispatch_class::typecheck(ClassTable *classT,
                               SymbolTable<Symbol, method_class> *methods,
                               SymbolTable<Symbol, Symbol> *objects)
{
    expr->typecheck(classT, methods, objects);
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
    {
        Expression_class *e = actual->nth(i);
        e->typecheck(classT, methods, objects);
    }
    // return should be return type of method looked up
    // in methods table - which I haven't yet populated :(
    set_type(Object);
    return Object;
}

Symbol cond_class::typecheck(ClassTable *classT,
                           SymbolTable<Symbol, method_class> *methods,
                           SymbolTable<Symbol, Symbol> *objects)
{
    pred->typecheck(classT, methods, objects);
    then_exp->typecheck(classT, methods, objects);
    else_exp->typecheck(classT, methods, objects);
    // return LUB of the type of then_exp and else_exp
    set_type(Object);
    return Object;
}

Symbol loop_class::typecheck(ClassTable *classT,
                           SymbolTable<Symbol, method_class> *methods,
                           SymbolTable<Symbol, Symbol> *objects)
{
    pred->typecheck(classT, methods, objects);
    body->typecheck(classT, methods, objects);
    set_type(Object);
    return Object;
}

Symbol typcase_class::typecheck(ClassTable *classT,
                              SymbolTable<Symbol, method_class> *methods,
                              SymbolTable<Symbol, Symbol> *objects)
{
    return expr->typecheck(classT, methods, objects);
}

Symbol block_class::typecheck(ClassTable *classT,
                            SymbolTable<Symbol, method_class> *methods,
                            SymbolTable<Symbol, Symbol> *objects)
{
    Symbol s;
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        Expression_class *e = body->nth(i);
        s = e->typecheck(classT, methods, objects);
    }
    // return last expression's type
    set_type(s);
    return s;
}

Symbol let_class::typecheck(ClassTable *classT,
                          SymbolTable<Symbol, method_class> *methods,
                          SymbolTable<Symbol, Symbol> *objects)
{
    init->typecheck(classT, methods, objects);
    Symbol s = body->typecheck(classT, methods, objects);
    set_type(s);
    return s;
}

Symbol plus_class::typecheck(ClassTable *classT,
                           SymbolTable<Symbol, method_class> *methods,
                           SymbolTable<Symbol, Symbol> *objects)
{
    e1->typecheck(classT, methods, objects);
    e2->typecheck(classT, methods, objects);
    // check that e1 and e2 are type Int, error if not
    set_type(Int);
    return Int;
}

Symbol sub_class::typecheck(ClassTable *classT,
                          SymbolTable<Symbol, method_class> *methods,
                          SymbolTable<Symbol, Symbol> *objects)
{
    e1->typecheck(classT, methods, objects);
    e2->typecheck(classT, methods, objects);
    // check that e1 and e2 are type Int, error if not
    set_type(Int);
    return Int;
}

Symbol mul_class::typecheck(ClassTable *classT,
                          SymbolTable<Symbol, method_class> *methods,
                          SymbolTable<Symbol, Symbol> *objects)
{
    e1->typecheck(classT, methods, objects);
    e2->typecheck(classT, methods, objects);
    // check that e1 and e2 are type Int, error if not
    set_type(Int);
    return Int;
}

Symbol divide_class::typecheck(ClassTable *classT,
                             SymbolTable<Symbol, method_class> *methods,
                             SymbolTable<Symbol, Symbol> *objects)
{
    e1->typecheck(classT, methods, objects);
    e2->typecheck(classT, methods, objects);
    // check that e1 and e2 are type Int, error if not
    set_type(Int);
    return Int;
}

Symbol neg_class::typecheck(ClassTable *classT,
                          SymbolTable<Symbol, method_class> *methods,
                          SymbolTable<Symbol, Symbol> *objects)
{
    e1->typecheck(classT, methods, objects);
    // check that e1 is type Int, error if not
    set_type(Int);
    return Int;
}

Symbol lt_class::typecheck(ClassTable *classT,
                         SymbolTable<Symbol, method_class> *methods,
                         SymbolTable<Symbol, Symbol> *objects)
{
    e1->typecheck(classT, methods, objects);
    e2->typecheck(classT, methods, objects);
    // check that e1 and e2 are type Int, error if not
    set_type(Bool);
    return Bool;
}

Symbol eq_class::typecheck(ClassTable *classT,
                         SymbolTable<Symbol, method_class> *methods,
                         SymbolTable<Symbol, Symbol> *objects)
{
    e1->typecheck(classT, methods, objects);
    e2->typecheck(classT, methods, objects);
    // check that e1 and e2 are same type, error if not
    set_type(Bool);
    return Bool;
}

Symbol leq_class::typecheck(ClassTable *classT,
                          SymbolTable<Symbol, method_class> *methods,
                          SymbolTable<Symbol, Symbol> *objects)
{
    e1->typecheck(classT, methods, objects);
    e2->typecheck(classT, methods, objects);
    // check that e1 and e2 are type Int, error if not
    set_type(Bool);
    return Bool;
}

Symbol comp_class::typecheck(ClassTable *classT,
                           SymbolTable<Symbol, method_class> *methods,
                           SymbolTable<Symbol, Symbol> *objects)
{
    e1->typecheck(classT, methods, objects);
    // check that e1 is type Bool, error if not
    set_type(Bool);
    return Bool;
}

Symbol int_const_class::typecheck(ClassTable *classT,
                                SymbolTable<Symbol, method_class> *methods,
                                SymbolTable<Symbol, Symbol> *objects)
{
    set_type(Int);
    return Int;
}

Symbol bool_const_class::typecheck(ClassTable *classT,
                                 SymbolTable<Symbol, method_class> *methods,
                                 SymbolTable<Symbol, Symbol> *objects)
{
    set_type(Bool);
    return Bool;
}

Symbol string_const_class::typecheck(ClassTable *classT,
                                   SymbolTable<Symbol, method_class> *methods,
                                   SymbolTable<Symbol, Symbol> *objects)
{
    set_type(Str);
    return Str;
}

Symbol new__class::typecheck(ClassTable *classT,
                           SymbolTable<Symbol, method_class> *methods,
                           SymbolTable<Symbol, Symbol> *objects)
{
    // check that the class exists
    // if not, error
    // otherwise, the type
    set_type(getTypeName());
    return getTypeName();
}

Symbol isvoid_class::typecheck(ClassTable *classT,
                             SymbolTable<Symbol, method_class> *methods,
                             SymbolTable<Symbol, Symbol> *objects)
{
    e1->typecheck(classT, methods, objects);
    set_type(Bool);
    return Bool;
}

Symbol no_expr_class::typecheck(ClassTable *classT,
                              SymbolTable<Symbol, method_class> *methods,
                              SymbolTable<Symbol, Symbol> *objects)
{
    set_type(No_type);
    return No_type;
}

Symbol object_class::typecheck(ClassTable *classT,
                             SymbolTable<Symbol, method_class> *methods,
                             SymbolTable<Symbol, Symbol> *objects)
{
    // check for object in objects table - not populated :(
    // if not found, error
    // otherwise, return the found type
    set_type(getName());
    return getName();
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    if (inheritanceCheck(classtable))
    {
        gatherDeclarations(classtable);
        SymbolTable<Symbol, method_class> *methods = new SymbolTable<Symbol, method_class>();
        SymbolTable<Symbol, Symbol> *objects = new SymbolTable<Symbol, Symbol>();
        typecheck(classtable, methods, objects);
    }

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


