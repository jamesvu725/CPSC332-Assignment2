#include <iostream>
#include <string>
#include <vector>
#include <utility>
#include <fstream>
#include <cerrno>
#include <ctype.h>
#include <iterator>
using namespace std;

int term(int count);
int termprime(int count);
int expression(int count);
int expressionprime(int count);
int statement(int count);

string keywords[] = { "int", "float", "bool", "if", "else", "then", "endif",
                      "while", "whileend", "do", "doend", "for", "forend",
                      "input", "output", "and", "or", "function", "begin", "end" };
char sep[] = { '\'', '(', ')', '{', '}', ',', ':', ';' };
char op[]  = { '*', '+', '-', '=', '/', '>', '<', '%'  };
vector<pair<string,string>> tok_lex;
vector<string> syn_vec;
vector<string> temp_str;
string statement_string = "<Statement> -> <Assignment Statement> | <Declarative Statement> | if <Conditional> then <Statement> else <Statement> endif | if <Conditional> then <Statement> endif | begin <Statement> <MoreStatements> end | while <Conditional> do <Statement> whileend";
string assignment_string = "<Assignment Statement> -> <Identifier> = <Expression>;";
string expression_string = "<Expression> -> <Term> <ExpressionPrime>";
string expressionprime_string = "<ExpressionPrime> -> + <Expression> | - <Expression> | Epsilon";
string term_string = "<Term> -> <Factor> <TermPrime>";
string termprime_string = "<TermPrime> -> * <Term> | / <Term> | Epsilon";
string factor_string = "<Factor> -> ( <Expression> ) | <Identifier> | <Number>";
string declarative_string = "<Declarative Statement> -> <Type> <Identifier> <MoreIDs>;";
string type_string = "<Type> -> int | float | bool";
string moreids_string = "<MoreIDs> -> , <Identifier> <MoreIDs> | Epsilon";
string conditional_string = "<Conditional> -> <Expression> <RelationalOperator> <Expression> | <Expression>";
string relop_string = "<RelationalOperator> -> < | <= | == | <> | >= | >";
string morestatements_string = "<MoreStatements> -> ; <Statement> <More Statements | epsilon";

string get_file_contents(string filename) {
  ifstream in(filename, ios::in | ios::binary);
  if (in) {
    string contents;
    in.seekg(0, in.end);
    contents.resize(in.tellg());
    in.seekg(0, in.beg);
    in.read(&contents[0], contents.size());
    in.close();
    return contents;
  }
  throw(errno);
}

bool isseperator(const char ch) {
  for (unsigned int i = 0; i < sizeof(sep); ++i) {
    if (ch == sep[i]) return true;
  }
  return false;
}

bool isoperator(const char ch) {
  for (unsigned int i = 0; i < sizeof(op); ++i) {
    if (ch == op[i]) return true;
  }
  return false;
}

int cur_char(const char ch) {
  if (isalpha(ch))       { return 1; }
  if (isdigit(ch))       { return 2; }
  if (ch == '.')         { return 3; }
  if (ch == '$')         { return 4; }
  if (isseperator(ch))   { return 5; }
  if (isoperator(ch))    { return 6; }
  if (ch == '!')         { return 7; }
  return 0;
}

void add_token_lexeme(const string token, const string lexeme) {
  pair <string, string> token_lexeme(token, lexeme);
  tok_lex.push_back(token_lexeme);
}

bool check_keyword(const string buf) {
  for (unsigned int i = 0; i < sizeof(keywords); ++i) {
    if (keywords[i] == buf) { return true; }
  }
  return false;
}

void add_keyword_or_identifier(const string buf, const bool flag) {
  if (flag) { add_token_lexeme("Keyword", buf); return; }
  add_token_lexeme("Identifier", buf);
}

void add_realfloat_or_int(const string buf, const bool flag) {
  if (flag) { add_token_lexeme("Real Float", buf); return; }
  add_token_lexeme("Integer", buf);
}

void lexer(const string con) {
  string char_buffer; //initialize character buffer
  int lex_state = 1, char_state = 0; // initial states
  bool backup = 0, float_flag = 0, end_flag = 0; //set flags
  auto it = con.begin();
  while (it != con.end() || !end_flag) { //iterate through string and make sure to process last character
    char_state = cur_char(*it); //get current character state
    switch (lex_state) {
      case 1: // starting state
        switch (char_state) {
          case 1:  lex_state = 2; break; // alphabet
          case 2:  lex_state = 4; break; // digits
          case 3:  lex_state = 7; break; // period
          case 4:  lex_state = 1; break; // $
          case 5:  lex_state = 7; break; // separators
          case 6:  lex_state = 8; break; // operators
          case 7:  lex_state = 9; break; // ! comments
          default: lex_state = 1;        // other
        }
        char_buffer = *it; // sets character buffer to current character
        backup = 0; // resets backup
        break;
      case 2: // in identifier or possible keyword
        if (char_state == 1 || char_state == 2 || char_state == 4) {
          lex_state = 2;      // alpha, digit, or $
          char_buffer += *it; // add to buffer
          break;
        }
        lex_state = 3;      // end of identifier
        backup = 1;         // do not increment it
        break;
      case 3: // end of identifier or keyword
        lex_state = 1; // reset lex state
        add_keyword_or_identifier(char_buffer, check_keyword(char_buffer));
        break;
      case 4: // in number
        if (char_state == 2) {        // digits
          lex_state = 4;              // in number
          char_buffer += *it;         // add to buffer
        } else if (char_state == 3) { // period
          lex_state = 5;              // in number after decimal
          char_buffer += *it;         // add to buffer
          float_flag = 1;               // sets flag
        } else {
          lex_state = 6;              // end of number
          backup = 1;                 // do not increment to next char
        }
        break;
      case 5: // in number after decimal
        if (char_state == 2) { // digits
          lex_state = 5;       // stay in same state
          char_buffer += *it;  // add to buffer
          break;
        }
        lex_state = 6;       // not digit, end of number
        backup = 1;          // do not increment to next char
        break;
      case 6: // end of number
        lex_state = 1; // reset lex state
        add_realfloat_or_int(char_buffer, float_flag);
        float_flag = 0;
        break;
      case 7: // separator
        lex_state = 1; // reset lex state
        add_token_lexeme("Separator", char_buffer);
        backup = 1; // do not increment to next char
        break;
      case 8: // operator
        lex_state = 1; // reset lex state
        add_token_lexeme("Operator", char_buffer);
        backup = 1; // set flag
        break;
      case 9: // in commment
        if (char_state == 7) {
          lex_state = 10; // end comment
          break;
        }
        lex_state = 9; // stay in comment
        break;
      case 10: // end comment
        lex_state = 1; // reset lex state
        break;
      default:
        cout << "Invalid lex state\n";
    }
    if (it == con.end()) { end_flag = 1; } // set end flag to stop loop
    if (!backup && it != con.end()) { ++it; } // increment to next char if not end or backup
  }
  switch (lex_state) {
    case 2: case 4: case 5: case 9:
      cout << "Did not end in final state!";
  }
}

void print_token_lexeme(string filename) {
  string output = "lexer_output_of_";
  output += filename;
  ofstream outfile;
  outfile.open(output);
  outfile << "Tokens\t\tLexemes\n";
  for (auto it = tok_lex.begin(); it != tok_lex.end(); ++it) {
    outfile << it->first << " \t" << it->second << "\n";
  }
  outfile.close();
}

void push_tok_lex(int count) {
  temp_str.push_back("\nToken: " + tok_lex[count].first + " \tLexeme: " + tok_lex[count].second);
}

bool identifier(int count) {
  return tok_lex[count].first == "Identifier";
}

bool num(int count) {
  return tok_lex[count].first == "Real Float" || tok_lex[count].first == "Integer";
}

int factor(int count) {
  int counter = count;
  temp_str.push_back(factor_string);
  if (tok_lex[counter].second == "(") {
    counter++;
    push_tok_lex(counter);
    if ((count = expression(counter)) > 0) {
      count++;
      push_tok_lex(count);
      if (tok_lex[count].second == ")") {
        return count;
      } else {
        return -1;
      }
    } else {
      return -1;
    }
  } else if (identifier(counter)) {
    return counter;
  } else if (num(counter)) {
    return counter;
  }
  return -1;
}

int termprime(int count) {
  temp_str.push_back(termprime_string);
  count++;
  push_tok_lex(count);
  if ((count = term(count)) > 0) { return count; }
  return -1;
}

int term(int count) {
  int counter = count;
  temp_str.push_back(term_string);
  if ((count = factor(count)) > 0) {
    counter = count;
    count++;
    if (tok_lex[count].second == "*" || tok_lex[count].second == "/") {
      push_tok_lex(count);
      if ((count = termprime(count)) > 0) {
        return count;
      } else {
        return -1;
      }
    }
  }
  return counter;
}

int expressionprime(int count) {
  temp_str.push_back(expressionprime_string);
  count++;
  push_tok_lex(count);
  if ((count = expression(count)) > 0) {
    return count;
  }
  return -1;
}

int expression(int count) {
  int counter = count;
  temp_str.push_back(expression_string);
  if ((count = term(count)) > 0) {
    counter = count;
    count++;
    if (tok_lex[count].second == "+" || tok_lex[count].second == "-") {
      push_tok_lex(count);
      if ((count = expressionprime(count)) > 0) {
        return count;
      } else {
        return -1;
      }
    }
  }
  return counter;
}

int assignment(int count) {
  if (identifier(count)) {
    temp_str.push_back(statement_string);
    temp_str.push_back(assignment_string);
    count++;
    push_tok_lex(count);
    if (tok_lex[count].second == "=") {
      count++;
      push_tok_lex(count);
      if ((count = expression(count)) > 0) {
        count++;
        push_tok_lex(count);
        if (tok_lex[count].second == ";") {
          return count;
        }
      }
    }
  }
  return -1;
}

bool type(int count) {
  return tok_lex[count].second == "int" || tok_lex[count].second == "float" || tok_lex[count].second == "bool";
}

int moreids(int count) {
  temp_str.push_back(moreids_string);
  if (tok_lex[count].second == ",") {
    count++;
    push_tok_lex(count);
    if (identifier(count)) {
      count++;
      push_tok_lex(count);
      if ((count = moreids(count)) > 0) {
        return count;
      } else { return -1; }
    } else { return -1; }
  }
  return count;
}

int declarative(int count) {
  if (type(count)) {
    temp_str.push_back(statement_string);
    temp_str.push_back(declarative_string);
    temp_str.push_back(type_string);
    count++;
    push_tok_lex(count);
    if (identifier(count)) {
      count++;
      push_tok_lex(count);
      if ((count = moreids(count)) > 0) {
        if (tok_lex[count].second == ";") {
          return count;
        }
      }
    }
  }
  return -1;
}

void add_temp_to_syn() {
  syn_vec.insert(
    syn_vec.end(),
    make_move_iterator(temp_str.begin()),
    make_move_iterator(temp_str.end())
  );
  temp_str.clear();
}

int morestatements(int count) {
  if (tok_lex[count].second == ";") {
    count++;
    temp_str.push_back(morestatements_string);
    push_tok_lex(count);
    add_temp_to_syn();
    if ((count = statement(count)) > 0) {
      count++;
      push_tok_lex(count);
      if ((count = morestatements(count)) > 0) {
        return count;
      } else { return -1; }
    } else { return -1; }
  }
  syn_vec.push_back(morestatements_string);
  return count;
}

int relop(int count) {
  int counter = count;
  if (tok_lex[count].second == "<" || tok_lex[count].second == ">" || tok_lex[count].second == "=") {
    push_tok_lex(count);
    temp_str.push_back(relop_string);
    count++;
    if (tok_lex[count].second == "<" || tok_lex[count].second == ">" || tok_lex[count].second == "=") {
      counter++;
      push_tok_lex(count);
    }
    return counter;
  } else {
    return -1;
  }
  return -1;
}

int condition(int count) {
  temp_str.push_back(conditional_string);
  int counter = count;
  if ((count = expression(count)) > 0) {
    counter = count;
    count++;
    if ((count = relop(count)) > 0) {
      count++;
      push_tok_lex(count);
      if ((count = expression(count)) > 0) {
        return count;
      } else { return -1; }
    }
    return counter;
  }
  return counter;
}

int statement(int count) {
  int counter = count;
  if ((count = assignment(counter)) > 0) {
    add_temp_to_syn();
    return count;
  } else if ((count = declarative(counter)) > 0) {
    add_temp_to_syn();
    return count;
  } else if (tok_lex[counter].second == "begin") {
    syn_vec.push_back(statement_string);
    counter++;
    push_tok_lex(counter);
    add_temp_to_syn();
    if ((count = statement(counter)) > 0) {
      add_temp_to_syn();
      count++;
      push_tok_lex(count);
      if ((count = morestatements(count)) > 0) {
        if (tok_lex[count].second == "end") {
          add_temp_to_syn();
          return count;
        }
      }
    }
  } else if (tok_lex[counter].second == "while") {
    // syn_vec.push_back(statement_string);
    counter++;
    push_tok_lex(counter);
    if ((count = condition(counter)) > 0) {
      count++;
      push_tok_lex(count);
      if (tok_lex[count].second == "do") {
        count++;
        push_tok_lex(count);
        if ((count = statement(count)) > 0) {
          count++;
          push_tok_lex(count);
          if (tok_lex[count].second == "whileend") {
            add_temp_to_syn();
            return count;
          } else {
            return -1;
          }
        } else {
          return -1;
        }
      } else {
        return -1;
      }
    }
  } else if (tok_lex[counter].second == "if") {
    // syn_vec.push_back(statement_string);
    counter++;
    push_tok_lex(counter);
    if ((count = condition(counter)) > 0) {
      count++;
      push_tok_lex(count);
      if (tok_lex[count].second == "then") {
        count++;
        push_tok_lex(count);
        if ((count = statement(count)) > 0) {
          count++;
          push_tok_lex(count);
          if (tok_lex[count].second == "else") {
            count++;
            push_tok_lex(count);
            if ((count = statement(count)) > 0) {
              count++;
              push_tok_lex(count);
              if (tok_lex[count].second == "endif") {
                add_temp_to_syn();
                return count;
              } else {
                return -1;
              }
            } else {
              return -1;
            }
          } else if (tok_lex[count].second == "endif") {
            add_temp_to_syn();
            return count;
          } else {
            return -1;
          }
        } else {
          return -1;
        }
      } else {
        return -1;
      }
    } else {
      return -1;
    }
  }
  temp_str.clear();
  return -1;
}

void syntax() {
  int counter = 0;
  push_tok_lex(counter);
  add_temp_to_syn();
  if ((counter = statement(counter)) > 0) {
    syn_vec.push_back("Syntax is correct.\n");
  } else {
    syn_vec.push_back("Syntactical analysis encountered an error.\n");
  }
}

void print_syntax(string filename) {
  string output = "syntax_output_of_";
  output += filename;
  ofstream outfile;
  outfile.open(output);
  for (auto it = syn_vec.cbegin(); it != syn_vec.cend(); ++it) {
    outfile << *it << "\n";
  }
  outfile.close();
}

int main(int argc, char* argv[]) {
  string content, filename;
  cin >> filename;
  content = get_file_contents(filename);
  lexer(content);
  print_token_lexeme(filename);
  syntax();
  print_syntax(filename);
  return 0;
}
