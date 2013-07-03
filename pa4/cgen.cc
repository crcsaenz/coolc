
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;
int CgenNode::labelIndex = 0;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  if (cgen_debug) os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  if (cgen_debug) os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Pop a value off of the stack into a register.  Stack address increases.
//
static void emit_pop(char *reg, ostream& str)
{
  emit_load(reg,1,SP,str);
  emit_addiu(SP,SP,4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}


///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/

      s << "String_dispTab" << endl;                          // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/

      s << "Int_dispTab" << endl;                         // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/

      s << "Bool_dispTab" << endl;                          // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

// emit code for the global class name table
void CgenClassTable::code_class_name_table()
{
    if (cgen_debug) str << "# start of class name table code\n";
    
    str << CLASSNAMETAB << LABEL;
    code_class_name_table(root());
    
    if (cgen_debug) str << "# end of class name table code\n\n";
}
void CgenClassTable::code_class_name_table(CgenNodeP nd)
{
    // get string constant entry
    StringEntry *strEnt = stringtable.lookup_string(nd->name->get_string());
    
    str << WORD;
    strEnt->code_ref(str);
    str << endl;
    
    // store class in global map for dispatch
    classNameMap.insert(std::pair<Symbol, CgenNodeP>(nd->name, nd));
    
    // recurse
    for (List<CgenNode> *l = nd->get_children(); l; l = l->tl())
        code_class_name_table(l->hd());
}

// emit code for the global class object table
void CgenClassTable::code_class_obj_table()
{
    if (cgen_debug) str << "# start of class obj table code\n";
    
    str << CLASSOBJTAB << LABEL;
    code_class_obj_table(root());
    
    if (cgen_debug) str << "# end of class obj table code\n\n";
}
void CgenClassTable::code_class_obj_table(CgenNodeP nd)
{
    // emit prototype object reference
    str << WORD;
    emit_protobj_ref(nd->name, str);
    str << endl;
    // emit object initializer reference
    str << WORD;
    emit_init_ref(nd->name, str);
    str << endl;
    
    // recurse
    for (List<CgenNode> *l = nd->get_children(); l; l = l->tl())
        code_class_obj_table(l->hd());
}

// emit code for the class dispatch tables
void CgenClassTable::code_dispatch_tables()
{
    if (cgen_debug) str << "# start of dispatch tables code\n";
    
    code_dispatch_table(root());
    
    if (cgen_debug) str << "# end of dispatch tables code\n\n";
}
void CgenClassTable::code_dispatch_table(CgenNodeP nd)
{
    emit_disptable_ref(nd->name, str);
    str << LABEL;
    
    // set up vector of nd's inherited classes (including itself)
    if (cgen_debug)
        cout << nd->name->get_string() << " inherited classes:" << endl;
    std::vector<CgenNodeP> inherited;
    for (CgenNodeP currNode = nd; currNode->name != No_class; currNode = currNode->get_parentnd()) {
        inherited.push_back(currNode);
        if (cgen_debug)
            cout << "\t" << currNode->name->get_string() << endl;
    }
    
    // inherited.back() should be Object in all cases
    if (cgen_debug)
        cout << "\tgreatest ancestor: " << inherited.back()->name->get_string() << endl;
    // add all inherited class methods to dispatch table
    int methodIndex = 0;
    for (int i = inherited.size()-1; i >= 0; i--) {
        CgenNodeP node = inherited[i];
        Features features = node->features;
        // check if feature is a method, then emit reference
        for (int j = features->first(); features->more(j); j = features->next(j)) {
            if (features->nth(j)->featureType == FEATURE_TYPE_METHOD) {
                method_class *m = (method_class *)features->nth(j);
                str << WORD;
                emit_method_ref(node->name, m->name, str);
                str << endl;
                nd->methodIndexMap.insert(std::pair<Symbol, int>(m->name, methodIndex));
                if (cgen_debug)
                    cout << nd->name->get_string() << " method index: " << methodIndex << endl;
                methodIndex++;
            }
        }
    }
    
    // recurse
    for (List<CgenNode> *l = nd->get_children(); l; l = l->tl())
        code_dispatch_table(l->hd());
}

// emit code for all class' prototype objects
void CgenClassTable::code_proto_objects()
{
    if (cgen_debug) str << "# start of prototype objects code\n";
    
    code_proto_object(root());
    
    if (cgen_debug) str << "# end of prototype objects code\n\n";
}
void CgenClassTable::code_proto_object(CgenNodeP nd)
{
    // garbage collection
    str << WORD << "-1" << endl;
    // emit the label
    emit_protobj_ref(nd->name, str);
    str << LABEL;
    
    // set up vector of nd's inherited classes (including itself)
    std::vector<CgenNodeP> inherited;
    for (CgenNodeP currNode = nd; currNode->name != No_class; currNode = currNode->get_parentnd())
        inherited.push_back(currNode);
    
    // set up vector of nd's inherited attributes (including its own)
    std::vector<attr_class*> attrs;
    for (int i = inherited.size()-1; i >= 0; i--) {
        CgenNodeP node = inherited[i];
        Features features = node->features;
        // check if feature is an attr, then add to vector
        for (int j = features->first(); features->more(j); j = features->next(j)) {
            if (features->nth(j)->featureType == FEATURE_TYPE_ATTR) {
                attr_class *a = (attr_class *)features->nth(j);
                attrs.push_back(a);
            }
        }
    }
    
    // emit class tag, object size, and dispatch table reference
    str << WORD << nd->classTag << endl;
    str << WORD << (attrs.size() + 3) << endl;
    str << WORD;
    emit_disptable_ref(nd->name, str);
    str << endl;
    if (cgen_debug)
        cout << nd->name->get_string() << " attribute indices:" << endl;
    // emit all attributes
    for (unsigned int i = 0; i < attrs.size(); i++) {
        attr_class *a = attrs[i];
        str << WORD;
        // emit default value based on type
        if (a->type_decl == Int) {
            IntEntry *intEnt = inttable.lookup_string("0");
            intEnt->code_ref(str);
        } else if (a->type_decl == Bool) {
            falsebool.code_ref(str);
        } else if (a->type_decl == Str) {
            StringEntry *strEnt = stringtable.lookup_string("");
            strEnt->code_ref(str);
        } else {
            // default of all other classes is void (null char or "0"?)
            str << 0;
        }
        str << endl;
        nd->attrIndexMap.insert(std::pair<Symbol, int>(a->name, i+3));
        if (cgen_debug)
            cout << "\t" << a->name->get_string() << ": " << (i+3) << endl;
    }
    
    // recurse
    for (List<CgenNode> *l = nd->get_children(); l; l = l->tl())
        code_proto_object(l->hd());
}

// emit code for all class' object initializers
void CgenClassTable::code_initializers()
{
    if (cgen_debug) str << "# start of initializers code\n";
    
    code_initializer(root());
    
    if (cgen_debug) str << "# end of initializers code\n\n";
}
void CgenClassTable::code_initializer(CgenNodeP nd)
{
    // emit the label
    emit_init_ref(nd->name, str);
    str << LABEL;
    
    // push the frame pointer, self reg, and return address
    emit_push(FP, str);
    emit_push(SELF, str);
    emit_push(RA, str);
    // new frame pointer and store accum before parent call
    emit_addiu(FP, SP, WORD_SIZE, str);
    emit_move(SELF, ACC, str);
    
    // call parent's init method (handles inherited attr inits)
    if (nd->name != Object) {
        std::string parentName = nd->get_parentnd()->name->get_string();
        emit_jal((char *)(parentName + CLASSINIT_SUFFIX).c_str(), str);
    }
    // attribute initializers if present and not a basic class
    if (!nd->basic()) {
        Features features = nd->features;
        if (cgen_debug)
            cout << nd->name->get_string() << " attr inits:" << endl;
        for (int i = features->first(); features->more(i); i = features->next(i)) {
            if (features->nth(i)->featureType == FEATURE_TYPE_ATTR) {
                attr_class *a = (attr_class *)features->nth(i);
                // code initializer expression
                // if not present, no_expr::code() still does nothing
                a->init->code(str, nd);
                // store result (from accum) at attr offset
                std::map<Symbol, int>::iterator ind = nd->attrIndexMap.find(a->name);
                if (ind != nd->attrIndexMap.end()) {
                    emit_store(ACC, ind->second, SELF, str);
                    if (cgen_debug)
                        cout << "\t" << a->name->get_string() << ": " << ind->second << endl;
                }
            }
        }
    }
    
    // restore state and return to caller
    emit_move(ACC, SELF, str);
    emit_pop(RA, str);
    emit_pop(SELF, str);
    emit_pop(FP, str);
    emit_return(str);
    
    // recurse
    for (List<CgenNode> *l = nd->get_children(); l; l = l->tl())
        code_initializer(l->hd());
}

// emit code for all class' methods
void CgenClassTable::code_methods()
{
    if (cgen_debug) str << "# start of methods code\n";
    
    code_methods(root());
    
    if (cgen_debug) str << "# end of methods code\n\n";
}
void CgenClassTable::code_methods(CgenNodeP nd)
{
    // ignore basic classes, already in runtime environment
    if (!nd->basic()) {
        Features features = nd->features;
        if (cgen_debug)
            cout << nd->name->get_string() << " methods:" << endl;
        for (int i = features->first(); features->more(i); i = features->next(i)) {
            if (features->nth(i)->featureType == FEATURE_TYPE_METHOD) {
                method_class *m = (method_class *)features->nth(i);
                // emit the label
                emit_method_ref(nd->name, m->name, str);
                str << LABEL;
                
                // push the frame pointer, self reg, and return address
                emit_push(FP, str);
                emit_push(SELF, str);
                emit_push(RA, str);
                // new frame pointer and store accum before method call
                emit_addiu(FP, SP, WORD_SIZE, str);
                emit_move(SELF, ACC, str);
                
                // set up formals offsets
                if (cgen_debug)
                    cout << "\t" << m->name->get_string() << " formals:" << endl;
                Formals formals = m->formals;
                for (int j = formals->first(); formals->more(j); j = formals->next(j)) {
                    formal_class *f = (formal_class *)formals->nth(j);
                    if (cgen_debug)
                        cout << "\t\t" << f->name->get_string() << ": " << j << endl;
                }
                
                // code the method body
                m->expr->code(str, nd);
                
                // restore state and return to caller
                emit_move(ACC, SELF, str);
                emit_pop(RA, str);
                emit_pop(SELF, str);
                emit_pop(FP, str);
                emit_return(str);
            }
        }
    }
    
    // recurse
    for (List<CgenNode> *l = nd->get_children(); l; l = l->tl())
        code_methods(l->hd());
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
   stringclasstag = 4 /* Change to your String class tag here */;
   intclasstag =    2 /* Change to your Int class tag here */;
   boolclasstag =   3 /* Change to your Bool class tag here */;
    // counter for setting class tags
    currClassTag = 0;
    
   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();
    // set up unique class tags correctly for use in typecase
    set_class_tags();

   code();
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

//
// CgenClassTable::set_class_tags
//
// wrapper for recursive inheritance graph traversal
void CgenClassTable::set_class_tags()
{
    if (cgen_debug)
        cout << "The root is: " << root()->name->get_string() << endl; // sanity check
    set_class_tag(root());
}

void CgenClassTable::set_class_tag(CgenNodeP nd)
{
    nd->classTag = currClassTag++;
    if (cgen_debug)
        cout << nd->name->get_string() << " class tag: " << nd->classTag << endl;

    for (List<CgenNode> *l = nd->get_children(); l != NULL; l = l->tl())
        set_class_tag(l->hd());
    
    // store tag of lowest child for case set-up
    nd->lastChildTag = currClassTag-1;
    if (cgen_debug)
        cout << "lastChild(" << nd->name->get_string() << "): " << nd->lastChildTag << endl;
}



void CgenClassTable::code()
{
  if (cgen_debug) cout << "#### coding global data ####" << endl << endl;
  code_global_data();

  if (cgen_debug) cout << "#### choosing gc ####" << endl << endl;
  code_select_gc();

  if (cgen_debug) cout << "#### coding constants ####" << endl << endl;
  code_constants();

    // code global table stuffs
    if (cgen_debug) cout << "#### coding class name table ####" << endl << endl;
    code_class_name_table();
    
    if (cgen_debug) cout << "#### coding class obj table ####" << endl << endl;
    code_class_obj_table();
    
    if (cgen_debug) cout << "#### coding dispatch tables ####" << endl << endl;
    code_dispatch_tables();
    
    if (cgen_debug) cout << "#### coding prototype objects ####" << endl << endl;
    code_proto_objects();

  if (cgen_debug) cout << "#### coding global text ####" << endl << endl;
  code_global_text();

    // code object initializers and methods
    if (cgen_debug) cout << "#### coding initializers ####" << endl << endl;
    code_initializers();
    
    if (cgen_debug) cout << "#### coding methods ####" << endl << endl;
    code_methods();

}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

// stores current formal params when entering a method call
// -- index of param = offset from FP
std::vector<Expression> params;
// stores current local variables, scope enforced by order (closest scope near end)
// -- index of var = offset from FP
std::vector<Symbol> locals;

void assign_class::code(ostream &s, CgenNodeP nd) {
    // evaluate expression
    expr->code(s, nd);
    
    // check for object name in local variables
    for (int i = locals.size() - 1; i >= 0; i--) {
        if (locals[i] == name) {
            int offset = i;
            emit_store(ACC, offset+3, FP, s);
            return;
        }
    }
    
    // check for object name in formals
    for (int i = params.size() - 1; i >= 0; i--) {
        // ???
    }
    
    // otherwise, attribute
    int offset = nd->attrIndexMap.find(name)->second;
    emit_store(ACC, offset, SELF, s);
}

void static_dispatch_class::code(ostream &s, CgenNodeP nd) {
    // evaluate parameters in reverse order, storing each on the stack
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
        params.push_back(actual->nth(i));
    for (int i = params.size() - 1; i >= 0; i--) {
        params[i]->code(s, nd);
        emit_push(ACC, s);
    }
    
    int validLabel = CgenNode::labelIndex++;
    int endLabel = CgenNode::labelIndex++;
    
    // branch if object is valid (not void)
    expr->code(s, nd);
    emit_bne(ACC, ZERO, validLabel, s);
    
    // expr was void, dispatch abort
    emit_load_string(ACC, stringtable.lookup_string(nd->filename->get_string()), s);
    emit_load_imm(T1, get_line_number(), s);
    emit_jal("_dispatch_abort", s);
    emit_branch(endLabel, s);
    
    // expr was not void, perform the dispatch
    emit_label_def(validLabel, s);
    std::string dispTab = type_name->get_string();
    dispTab += DISPTAB_SUFFIX;
    emit_load_address(T1, (char *)dispTab.c_str(), s);
    // find method index for type_name class
    CgenNodeP node = classNameMap.find(type_name)->second;
    int offset = node->methodIndexMap.find(name)->second;
    emit_load(T1, offset, T1, s);
    // call method and fall through
    emit_jalr(T1, s);
    emit_label_def(endLabel, s);
    // clear params
    params.clear();
}

void dispatch_class::code(ostream &s, CgenNodeP nd) {
    // evaluate parameters in reverse order, storing each on the stack
    for (int i = actual->first(); actual->more(i); i = actual->next(i))
        params.push_back(actual->nth(i));
    for (int i = params.size() - 1; i >= 0; i--) {
        params[i]->code(s, nd);
        emit_push(ACC, s);
    }
    
    int validLabel = CgenNode::labelIndex++;
    int endLabel = CgenNode::labelIndex++;
    
    // branch if object is valid (not void)
    expr->code(s, nd);
    emit_bne(ACC, ZERO, validLabel, s);
    
    // expr was void, dispatch abort
    emit_load_string(ACC, stringtable.lookup_string(nd->filename->get_string()), s);
    emit_load_imm(T1, get_line_number(), s);
    emit_jal("_dispatch_abort", s);
    emit_branch(endLabel, s);
    
    // expr was not void, perform the dispatch
    emit_label_def(validLabel, s);
    emit_load(T1, DISPTABLE_OFFSET, ACC, s);     // ACC holds expr's return object
    // find method index for expr's class
    CgenNodeP node = classNameMap.find(expr->type)->second;
    int offset = node->methodIndexMap.find(name)->second;
    emit_load(T1, offset, T1, s);
    // call method and fall through
    emit_jalr(T1, s);
    emit_label_def(endLabel, s);
    params.clear();
}

void cond_class::code(ostream &s, CgenNodeP nd) {
    // evaluate predicate, store result on stack
    pred->code(s, nd);
    emit_push(ACC, s);
    
    int elseLabel = CgenNode::labelIndex++;
    int endLabel = CgenNode::labelIndex++;
    
    // branch on pred's boolean (int) value
    emit_pop(T1, s);    // contains pred's Bool object
    emit_fetch_int(T1, T1, s);
    emit_beqz(T1, elseLabel, s);
    
    // here e1 was true, so execute then branch and jump to end
    then_exp->code(s, nd);
    emit_branch(endLabel, s);
    
    // otherwise pred was false, so execute else branch and fall through
    emit_label_def(elseLabel, s);
    else_exp->code(s, nd);
    emit_label_def(endLabel, s);
}

void loop_class::code(ostream &s, CgenNodeP nd) {
    
    int startLabel = CgenNode::labelIndex++;
    int endLabel = CgenNode::labelIndex++;
    
    // branch on pred's boolean (int) value
    emit_label_def(startLabel, s);
    pred->code(s, nd);
    emit_fetch_int(T1, ACC, s);
    emit_beqz(T1, endLabel, s);
    
    // here pred was true, so execute the body and jump back to start
    body->code(s, nd);
    emit_branch(startLabel, s);
    
    // otherwise pred was false, so fall through and return void
    emit_label_def(endLabel, s);
    emit_load_imm(ACC, 0, s);
}

void typcase_class::code(ostream &s, CgenNodeP nd) {
    // evaluate the expression, store result on stack
    expr->code(s, nd);
    
    int startCaseLabel = CgenNode::labelIndex++;
    int endCaseLabel = CgenNode::labelIndex++;
    
    // check for valid object (non-void)
    emit_bne(ACC, ZERO, startCaseLabel, s);
    emit_load_string(ACC, stringtable.lookup_string(nd->filename->get_string()), s);
    emit_load_imm(T1, get_line_number(), s);
    emit_jal("_case_abort2", s);
    emit_branch(endCaseLabel, s);
    
    // sort cases by decreasing class tag
    std::vector<std::pair<int, int> > branchSortingMap;    // class tag -> caselist index
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        branch_class *b = (branch_class *)cases->nth(i);
        CgenNodeP node = classNameMap.find(b->type_decl)->second;
        branchSortingMap.push_back(std::pair<int, int>(node->classTag, i));
    }
    std::sort(branchSortingMap.begin(), branchSortingMap.end());
    
    emit_label_def(startCaseLabel, s);
    // TODO: iterate through branchSortingMap, emitting code for each case branch
    
    emit_label_def(endCaseLabel, s);
}

void block_class::code(ostream &s, CgenNodeP nd) {
    // code each expression
    for (int i = body->first(); body->more(i); i = body->next(i))
        body->nth(i)->code(s, nd);
}

void let_class::code(ostream &s, CgenNodeP nd) {
    // if no initialization, create prototype object
    if (init->exprType == EXPR_TYPE_NO_EXPR) {
        // get class tag
        if (type_decl == SELF_TYPE) {
            emit_load(T1, TAG_OFFSET, SELF, s);
        } else {
            CgenNodeP node = classNameMap.find(type_decl)->second;
            emit_load_imm(T1, node->classTag, s);
        }
        // load object table address
        emit_load_address(T2, CLASSOBJTAB, s);
        // create new class prototype object
        emit_load_imm(T3, 8, s);    // each class uses 2 words (8 bytes) in objTab
        emit_mul(T1, T1, T3, s);    // now holds offset into objTab
        emit_addu(ACC, T2, T1, s);  // now ACC holds address to class_protObj
        emit_jal("Object.copy", s);
        // call class' init function (one word after protObj)
        emit_load(T1, 1, ACC, s);
        emit_jal(T1, s);

    } else {
        init->code(s, nd);
    }
    // store local var on stack and put in scope
    emit_push(ACC, s);
    locals.push_back(identifier);
    // emit code for the body
    body->code(s, nd);
    // restore stack and remove the local var from scope
    emit_pop(T1, s);
    locals.pop_back();
}

void plus_class::code(ostream &s, CgenNodeP nd) {
    // evaluate first expr, store result on stack
    e1->code(s, nd);
    emit_push(ACC, s);
    // evaluate second expr, store result on stack
    e2->code(s, nd);
    emit_push(ACC, s);
    
    // create new Int prototype object
    std::string intProtObj = "Int";
    intProtObj += PROTOBJ_SUFFIX;
    emit_load_address(ACC, (char *)intProtObj.c_str(), s);
    emit_jal("Object.copy", s);
    // compute sum
    emit_pop(T2, s);    // contains e2's Int object
    emit_pop(T1, s);    // contains e1's Int object
    emit_fetch_int(T2, T2, s);
    emit_fetch_int(T1, T1, s);
    emit_add(T3, T1, T2, s);    // T3 contains result value
    // store value in object
    emit_store(T3, DEFAULT_OBJFIELDS, ACC, s);
}

void sub_class::code(ostream &s, CgenNodeP nd) {
    // evaluate first expr, store result on stack
    e1->code(s, nd);
    emit_push(ACC, s);
    // evaluate second expr, store result on stack
    e2->code(s, nd);
    emit_push(ACC, s);
    
    // create new Int prototype object
    std::string intProtObj = "Int";
    intProtObj += PROTOBJ_SUFFIX;
    emit_load_address(ACC, (char *)intProtObj.c_str(), s);
    emit_jal("Object.copy", s);
    // compute difference
    emit_pop(T2, s);    // contains e2's Int object
    emit_pop(T1, s);    // contains e1's Int object
    emit_fetch_int(T2, T2, s);
    emit_fetch_int(T1, T1, s);
    emit_sub(T3, T1, T2, s);    // T3 contains result value
    // store value in object
    emit_store(T3, DEFAULT_OBJFIELDS, ACC, s);
}

void mul_class::code(ostream &s, CgenNodeP nd) {
    // evaluate first expr, store result on stack
    e1->code(s, nd);
    emit_push(ACC, s);
    // evaluate second expr, store result on stack
    e2->code(s, nd);
    emit_push(ACC, s);
    
    // create new Int prototype object
    std::string intProtObj = "Int";
    intProtObj += PROTOBJ_SUFFIX;
    emit_load_address(ACC, (char *)intProtObj.c_str(), s);
    emit_jal("Object.copy", s);
    // compute product
    emit_pop(T2, s);    // contains e2's Int object
    emit_pop(T1, s);    // contains e1's Int object
    emit_fetch_int(T2, T2, s);
    emit_fetch_int(T1, T1, s);
    emit_mul(T3, T1, T2, s);    // T3 contains result value
    // store value in object
    emit_store(T3, DEFAULT_OBJFIELDS, ACC, s);
}

void divide_class::code(ostream &s, CgenNodeP nd) {
    // evaluate first expr, store result on stack
    e1->code(s, nd);
    emit_push(ACC, s);
    // evaluate second expr, store result on stack
    e2->code(s, nd);
    emit_push(ACC, s);
    
    // create new Int prototype object
    std::string intProtObj = "Int";
    intProtObj += PROTOBJ_SUFFIX;
    emit_load_address(ACC, (char *)intProtObj.c_str(), s);
    emit_jal("Object.copy", s);
    // compute division
    emit_pop(T2, s);    // contains e2's Int object
    emit_pop(T1, s);    // contains e1's Int object
    emit_fetch_int(T2, T2, s);
    emit_fetch_int(T1, T1, s);
    emit_div(T3, T1, T2, s);    // T3 contains result value
    // store value in object
    emit_store(T3, DEFAULT_OBJFIELDS, ACC, s);
}

void neg_class::code(ostream &s, CgenNodeP nd) {
    // evaluate expr, store result on stack
    e1->code(s, nd);
    emit_push(ACC, s);
    
    // create new Int prototype object
    std::string intProtObj = "Int";
    intProtObj += PROTOBJ_SUFFIX;
    emit_load_address(ACC, (char *)intProtObj.c_str(), s);
    emit_jal("Object.copy", s);
    // compute negation
    emit_pop(T1, s);    // contains e1's Int object
    emit_fetch_int(T1, T1, s);
    emit_neg(T1, T1, s);    // T1 contains result value
    // store value in object
    emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void lt_class::code(ostream &s, CgenNodeP nd) {
    // evaluate first expr, store result on stack
    e1->code(s, nd);
    emit_push(ACC, s);
    // evaluate second expr, store result on stack
    e2->code(s, nd);
    emit_push(ACC, s);
    
    int lessThanLabel = CgenNode::labelIndex++;
    int endLabel = CgenNode::labelIndex++;
    
    // branch on e1 and e2's value
    emit_pop(T2, s);
    emit_fetch_int(T2, T2, s);
    emit_pop(T1, s);
    emit_fetch_int(T1, T1, s);
    emit_blt(T1, T2, lessThanLabel, s);
    
    // here e1 ~< e2, so emit a falsebool and jump to end
    emit_load_bool(ACC, falsebool, s);
    emit_branch(endLabel, s);
    
    // otherwise e1 < e2, so emit a truebool and fall through
    emit_label_def(lessThanLabel, s);
    emit_load_bool(ACC, truebool, s);
    emit_label_def(endLabel, s);
}

void eq_class::code(ostream &s, CgenNodeP nd) {
    // evaluate first expr, store result on stack
    e1->code(s, nd);
    emit_push(ACC, s);
    // evaluate second expr, store result on stack
    e2->code(s, nd);
    emit_push(ACC, s);
    
    int equalLabel = CgenNode::labelIndex++;
    int endLabel = CgenNode::labelIndex++;
    
    // branch on e1 and e2's pointers
    emit_pop(T2, s);
    emit_pop(T1, s);
    emit_beq(T1, T2, equalLabel, s);
    
    // here e1 ~= e2, so emit a falsebool, test primitive equality, then jump to end
    emit_load_bool(ACC, falsebool, s);
    emit_jal("equality_test", s);
    emit_branch(endLabel, s);
    
    // otherwise e1 = e2, so emit a truebool and fall through
    emit_label_def(equalLabel, s);
    emit_load_bool(ACC, truebool, s);
    emit_label_def(endLabel, s);
}

void leq_class::code(ostream &s, CgenNodeP nd) {
    // evaluate first expr, store result on stack
    e1->code(s, nd);
    emit_push(ACC, s);
    // evaluate second expr, store result on stack
    e2->code(s, nd);
    emit_push(ACC, s);
    
    int lessThanEqLabel = CgenNode::labelIndex++;
    int endLabel = CgenNode::labelIndex++;
    
    // branch on e1 and e2's value
    emit_pop(T2, s);
    emit_fetch_int(T2, T2, s);
    emit_pop(T1, s);
    emit_fetch_int(T1, T1, s);
    emit_bleq(T1, T2, lessThanEqLabel, s);
    
    // here e1 ~<= e2, so emit a falsebool and jump to end
    emit_load_bool(ACC, falsebool, s);
    emit_branch(endLabel, s);
    
    // otherwise e1 <= e2, so emit a truebool and fall through
    emit_label_def(lessThanEqLabel, s);
    emit_load_bool(ACC, truebool, s);
    emit_label_def(endLabel, s);
}

void comp_class::code(ostream &s, CgenNodeP nd) {
    // evaluate expr, store result on stack
    e1->code(s, nd);
    emit_push(ACC, s);
    
    int falseLabel = CgenNode::labelIndex++;
    int endLabel = CgenNode::labelIndex++;
    
    // branch on e1's boolean (int) value
    emit_pop(T1, s);    // contains e1's Bool object
    emit_fetch_int(T1, T1, s);
    emit_beqz(T1, falseLabel, s);
    
    // here e1 was true, so emit a falsebool and jump to end
    emit_load_bool(ACC, falsebool, s);
    emit_branch(endLabel, s);
    
    // otherwise e1 was false, so emit a truebool and fall through
    emit_label_def(falseLabel, s);
    emit_load_bool(ACC, truebool, s);
    emit_label_def(endLabel, s);
}

void int_const_class::code(ostream& s, CgenNodeP nd)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s, CgenNodeP nd)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s, CgenNodeP nd)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s, CgenNodeP nd) {
    // get class tag
    if (type_name == SELF_TYPE) {
        emit_load(T1, TAG_OFFSET, SELF, s);
    } else {
        CgenNodeP node = classNameMap.find(type_name)->second;
        emit_load_imm(T1, node->classTag, s);
    }
    // load object table address
    emit_load_address(T2, CLASSOBJTAB, s);
    // create new class prototype object
    emit_load_imm(T3, 8, s);    // each class uses 2 words (8 bytes) in objTab
    emit_mul(T1, T1, T3, s);        // now holds offset into objTab
    emit_addu(ACC, T2, T1, s);  // now ACC holds address to class_protObj
    emit_jal("Object.copy", s);
    // call class' init function (one word after protObj)
    emit_load(T1, 1, ACC, s);
    emit_jal(T1, s);
}

void isvoid_class::code(ostream &s, CgenNodeP nd) {
    // evaluate expr, store result on stack
    e1->code(s, nd);
    emit_push(ACC, s);
    
    int voidLabel = CgenNode::labelIndex++;
    int endLabel = CgenNode::labelIndex++;
    
    // branch on e1's void-ness
    emit_pop(T1, s);    // contains e1's object
    emit_beqz(T1, voidLabel, s);    // if object is 0, void
    
    // here e1 was not void, so emit a falsebool and jump to end
    emit_load_bool(ACC, falsebool, s);
    emit_branch(endLabel, s);
    
    // otherwise e1 was void, so emit a truebool and fall through
    emit_label_def(voidLabel, s);
    emit_load_bool(ACC, truebool, s);
    emit_label_def(endLabel, s);
}

void no_expr_class::code(ostream &s, CgenNodeP nd) {
    // nothing emitted here
}

void object_class::code(ostream &s, CgenNodeP nd) {
    if (name == self) {
        emit_move(ACC, SELF, s);
    } else {
        // check for object name in local variables
        for (int i = locals.size() - 1; i >= 0; i--) {
            if (locals[i] == name) {
                int offset = i;
                emit_store(ACC, offset+3, FP, s);
                return;
            }
        }
        
        // check for object name in formals
        for (int i = params.size() - 1; i >= 0; i--) {
            // ???
        }
        
        // otherwise, attribute
        int offset = nd->attrIndexMap.find(name)->second;
        emit_store(ACC, offset, SELF, s);
    }
}


