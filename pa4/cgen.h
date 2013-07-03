#include <assert.h>
#include <stdio.h>
#include <vector>
#include <algorithm>
#include <map>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
    int currClassTag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();
    // emit code for global tables (yay recursion!)
    void code_class_name_table();
    void code_class_name_table(CgenNodeP nd);
    void code_class_obj_table();
    void code_class_obj_table(CgenNodeP nd);
    void code_dispatch_tables();
    void code_dispatch_table(CgenNodeP nd);
    void code_proto_objects();
    void code_proto_object(CgenNodeP nd);
    // emit code for object inits and methods (yay more recursion!)
    void code_initializers();
    void code_initializer(CgenNodeP nd);
    void code_methods();
    void code_methods(CgenNodeP nd);

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
    void set_class_tags();
    void set_class_tag(CgenNodeP nd);

public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
    // range [classTag, lastChildTag] covers all classes <= this class
    int classTag, lastChildTag;
    // map from each attribute's name to its ABSOLUTE index in prototype object
    std::map<Symbol, int> attrIndexMap;
    // map frome ach method's name to its index in the dispatch table
    std::map<Symbol, int> methodIndexMap;
    // unique label index
    static int labelIndex;
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

// global class map from Symbol name to CgenNodeP
std::map<Symbol, CgenNodeP> classNameMap;

