#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
#include <map>
#include <set>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
    int semant_errors;
    void install_basic_classes();
    ostream& error_stream;

public:
    ClassTable(Classes);
    int errors() { return semant_errors; }
    ostream& semant_error();
    ostream& semant_error(Class_ c);
    ostream& semant_error(Symbol filename, tree_node *t);
    // maps child to parent
    std::map<Symbol, Symbol> *inheritanceMap;
    // maps class name to class data
    std::map<Symbol, class__class> *classEnv;
    // maps class name to methods map, which maps method name to method data
    std::map<Symbol, std::map<Symbol, method_class> > *methodEnv;
    // maps class name to attributes map, which maps attribute name to attribute data
    std::map<Symbol, std::map<Symbol, attr_class> > *attrEnv;
};


#endif

