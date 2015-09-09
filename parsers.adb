with Statements;                  use Statements;
with Statement_Lists;             use Statement_Lists;
with Arithmetic_Expressions;      use Arithmetic_Expressions;
with Operands;                    use Operands;
with If_Statements;               use If_Statements;
with While_Statements;            use While_Statements;
with Display_Statements;          use Display_Statements;
with Assignment_Statements;       use Assignment_Statements;
with Boolean_Expressions;         use Boolean_Expressions;
with Ids;                         use Ids;
with Binary_Expressions;          use Binary_Expressions;
with Literal_Integers;            use Literal_Integers;
with Unary_Expressions;           use Unary_Expressions;
with Binary_Expressions.Add_Expr; use Binary_Expressions.Add_Expr;
with Binary_Expressions.Div_Expr; use Binary_Expressions.Div_Expr;
with Binary_Expressions.Mul_Expr; use Binary_Expressions.Mul_Expr;
with Binary_Expressions.Sub_Expr; use Binary_Expressions.Sub_Expr;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Characters;
with Ada.Characters.Handling;
with Boolean_Expressions.EQ_Expr; use Boolean_Expressions.EQ_Expr;
with Boolean_Expressions.NE_Expr; use Boolean_Expressions.NE_Expr;
with Boolean_Expressions.GT_Expr; use Boolean_Expressions.GT_Expr;
with Boolean_Expressions.GE_Expr; use Boolean_Expressions.GE_Expr;
with Boolean_Expressions.LT_Expr; use Boolean_Expressions.LT_Expr;
with Boolean_Expressions.LE_Expr; use Boolean_Expressions.LE_Expr;
package body Parsers is

   function create_parser (file_name: in String) return Parser is
      par : Parser;
      lex : Lexical_Analyzer;
   begin
      lex := create_lexical_analyzer (file_name => file_name);
      par.lex := lex;
      return par;
   end create_parser;

   procedure parse (p: in out Parser; prog: out Program) is
      procedure getStatement_Lists (p: in out Parser; stmtlist : out Statement_List);
      procedure getStatement (p: in out Parser; stmtaccess : out Statement_Access);

      procedure Match (expected : in String) is
         token : Lexeme;
      begin
         get_token (p.lex, token);
         if token /= to_lexeme (s => expected) then
            raise parser_exception;
         end if;
      exception
         when parser_exception =>
            Put_Line ("error");

            -- do not want to handle the exception here since your program
            -- would continue executing

      end Match;
      procedure getId (p: in out Parser; id1: out Id) is
         token : lexeme;

      begin
         get_token (p.lex, token);
         if Ada.Characters.Handling.Is_Digit(token(1)) then
           raise parser_exception;
         end if;
         id1:= create_Id(token(1));
      exception
         when parser_exception =>
            Put_Line ("error");

            -- same comment

      end getId;
      procedure getLiteralInteger(p: in out Parser; litint: out Literal_Integer) is
         token : lexeme;
         n : integer;
      begin
         get_token(p.lex, token);
         n:= Integer'Value (String(token));
         litint:= create_Literal_integer(n);

      end getLiteralInteger;

      procedure get_Operand (p: in out Parser; op: out Operand_Access) is
         lit : Literal_Integer;
         token: lexeme;
         id1 : id;
         begin

         token := get_lookahead_token (p.lex);
         if  Ada.Characters.Handling.Is_Digit(token(1)) then
            op:= new Literal_Integer;
            getLiteralInteger (p, lit);
		op.all:= Operand'Class(lit);
         else
            op:= new id;
            getID(p, id1);
            op.all:= Operand'Class(id1);
         end if;

      end get_Operand;
   procedure createBinaryExpression(op1, op2: in operand_access; arithmeticop: in Arithmetic_Operator; expr: out Arithmetic_Expression_Access) is
     	 ad : Add_Expression;
         su : Sub_Expression;
         mu : Mul_Expression;
         di : Div_Expression;

      begin
         if arithmeticop = ADD then
            expr:= new Add_Expression;
            ad:= create_add_expression(op1,op2);
            expr.all:=Arithmetic_Expression'Class(ad);
         elsif arithmeticop = SUB then
            expr:= new sub_Expression;
            su:= create_sub_expression(op1,op2);
            expr.all:=Arithmetic_Expression'Class(su);
         elsif arithmeticop = MUL then
            expr:= new mul_Expression;
            mu:= create_mul_expression(op1,op2);
            expr.all:=Arithmetic_Expression'Class(mu);
         else
            expr:= new div_Expression;
            di:= create_div_expression(op1,op2);
            expr.all:=Arithmetic_Expression'Class(di);
         end if;
         end createBinaryExpression;
      procedure getArithmeticOperator(p: in out parser; arithop: out arithmetic_operator) is
         token: lexeme;
      begin

         get_token(p.lex,token);
         if token = to_lexeme(s => "+") then
            arithop := ADD;
         elsif token = to_lexeme(s => "-")then
            arithop := SUB;
         elsif token= to_lexeme (s => "*") then
            arithop:= MUL;
         elsif token= to_lexeme (s => "/") then
               arithop:=DIV;
         end if;


      end getArithmeticOperator;


      procedure getArithmetic_expression(p: in out Parser; arithmeticExpr : out Arithmetic_Expression_Access) is
         token : Lexeme;
         op: Arithmetic_Operator;
         op1 : operand_access;
         op2 : operand_access;
         expr : Unary_Expression;
      begin
         token := get_lookahead_token(p.lex);
         if token = to_lexeme(s => "+") or token = to_lexeme(s => "-") or token = to_lexeme(s => "*") or token = to_lexeme(s => "/") then
           getArithmeticOperator(p, op);
           get_Operand (p, op1);
           get_Operand(p, op2);
           createBinaryExpression(op1 ,op2,op, arithmeticExpr);

         else
            get_Operand(p,op1);
            Arithmeticexpr:= new Unary_Expression;
            expr:= create_unary_Expression(op1);
            ArithmeticExpr.all:=Arithmetic_Expression'class(expr);
         end if;
      end getArithmetic_expression;
     procedure getAssignment_Statement (p: in out Parser; assignstmt : out Assignment_Statement) is
         arithmeticexpr : Arithmetic_Expression_Access;
         id1: Id;
      begin
         getId (p, id1);
         Match ("<-");
         getArithmetic_expression (p, arithmeticexpr);
         assignstmt:= create_Assignment_Statement(id1 ,arithmeticexpr);

      end getAssignment_Statement;



      procedure getBoolean_expression (p: in out Parser; booleanstmt : out Boolean_Expression_Access) is
         op1 : Operand_Access;
         op2 : Operand_Access;
         boolEQ : EQ_Expression;
         boolNE: NE_Expression;
         boolGT: GT_Expression;
         boolGE: GE_Expression;
         boolLT: LT_Expression;
         boolLE: LE_Expression;
         token : lexeme;
      begin
        token := get_lookahead_token (p.lex);
         if token = to_lexeme(s => "=") then
           	match("=");
          	booleanstmt:= new EQ_Expression;
            	get_Operand (p, op1);
            	get_Operand (p, op2);
         	boolEQ := create_EQ_Expression(op1, op2);
            	booleanstmt.all:= Boolean_Expression'Class(boolEQ);
         elsif token = to_lexeme(s => ">=") then
            	match(">=");
           	booleanstmt:= new GE_Expression;
                get_Operand (p, op1);
         	get_Operand (p, op2);
         	boolGE := create_GE_Expression(op1, op2);
           	booleanstmt.all:= Boolean_Expression'Class(boolGE);
         elsif token = to_lexeme(s => ">") then
            	match(">");
           	booleanstmt:= new GT_Expression;
                get_Operand (p, op1);
         	get_Operand (p, op2);
         	boolGT := create_GT_Expression(op1, op2);
           	booleanstmt.all:= Boolean_Expression'Class(boolGT);
         elsif token = to_lexeme(s => "<=") then
            	match("<=");
            	booleanstmt:= new LE_Expression;
                get_Operand (p, op1);
         	get_Operand (p, op2);
         	boolLE := create_LE_Expression(op1, op2);
            	booleanstmt.all:= Boolean_Expression'Class(boolLE);
         elsif token = to_lexeme(s=> "<") then
            	match("<");
            	booleanstmt:= new LT_Expression;
                get_Operand (p, op1);
         	get_Operand (p, op2);
                boolLT := create_LT_Expression(op1,op2);
       		booleanstmt.all:= Boolean_Expression'Class(boolLT);
         elsif token = to_lexeme(s=> "/=") then
            	match("/=");
            	booleanstmt:= new NE_Expression;
                get_Operand (p, op1);
         	get_Operand (p, op2);
                boolNE := create_NE_Expression(op1,op2);
       		booleanstmt.all:= Boolean_Expression'Class(boolNE);
         end if;
          end getBoolean_expression;
      procedure getIf_Statement(p: in out Parser; ifstmt : out If_Statement) is
         boolexp     : Boolean_Expression_Access;
         stmtList_l1 : Statement_List;
         stmtList_l2 : Statement_List;
      begin
         Match ("if");
         getBoolean_expression (p, boolexp);
         Match ("then");
         getStatement_Lists (p, stmtList_l1);
         Match ("else");
         getStatement_Lists (p, stmtList_l2);
         Match ("end");
         ifstmt := create_if_statement (boolexp, stmtList_l1, stmtList_l2);
      end getIf_Statement;
      procedure getDisplay_Statement (p: in out Parser; displaystmt: out Display_Statement)is
         id1 : Id;
      begin

         Match ("display");
         Match ("(");
         getId (p, id1);
         Match (")");
         displaystmt := create_display_statement (id1);
      end getDisplay_Statement;

      procedure getWhile_Statement (p: in out Parser; whilestmt: out While_Statement)is
         booleanexpr : Boolean_Expression_Access;
         stmtList    : Statement_List;
      begin
         Match ("while");
         getBoolean_expression (p, booleanexpr);
         Match ("do");
         getStatement_Lists (p, stmtList);
         Match ("end");
         whilestmt := create_while_statement (booleanexpr, stmtList);
      end getWhile_Statement;

      procedure getStatement (p: in out Parser; stmtaccess: out Statement_Access)
      is
         token       : Lexeme;
         ifstmt      : If_Statement;
         whilestmt   : While_Statement;
         displaystmt : Display_Statement;
         assignstmt  : Assignment_Statement;
      begin
        token:= get_lookahead_token (p.lex);
         if token = to_lexeme (s => "if") then
            stmtaccess := new If_Statement;
            getIf_Statement (p, ifstmt);
            stmtaccess.all := Statement'Class (ifstmt);
         elsif token = to_lexeme (s => "while") then
            stmtaccess := new While_Statement;
            getWhile_Statement (p, whilestmt);
            stmtaccess.all := Statement'Class (whilestmt);
         elsif token = to_lexeme (s => "display") then
            stmtaccess := new Display_Statement;
            getDisplay_Statement (p, displaystmt);
            stmtaccess.all := Statement'Class (displaystmt);
         else
            stmtaccess := new Assignment_Statement;
            getAssignment_Statement (p, assignstmt);
            stmtaccess.all := Statement'Class (assignstmt);
         end if;
      end getStatement;
      procedure getStatement_Lists (p: in out Parser; stmtlist: out Statement_List)
      is
         stmtaccess : Statement_Access;
         token      : Lexeme;
      begin
         getStatement (p, stmtaccess);
         add (sl => stmtlist, s => stmtaccess);
         token := get_lookahead_token (p.lex);
         while token = to_lexeme (s => ";") loop
            Match (";");
            getStatement (p, stmtaccess);
            add (sl => stmtlist, s => stmtaccess);
            token := get_lookahead_token (p.lex);
         end loop;
      end getStatement_Lists;
      procedure getProgram (p: in out Parser; prog: out Program) is
         stmtlist : Statement_List;
      begin
         Match ("main");
         Match ("(");
         Match (")");
         getStatement_Lists (p, stmtlist);
         prog := create_program (stmtlist);
      end getProgram;
   begin
      getProgram(p,prog);
   end parse;
end Parsers;


-- 98/100 Please see my comments in your code.
