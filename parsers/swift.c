#include "general.h"	/* must always come first */

#include <string.h>

#include "keyword.h"
#include "debug.h"
#include "entry.h"
#include "parse.h"
#include "read.h"
#include "routines.h"
#include "selectors.h"
#include "trashbox.h"
#include "vstring.h"

typedef enum {
	K_PROTOCOL,
	K_METHOD,
	K_STATIC_METHOD,
	K_VAR,
	K_FIELD,
	K_FUNCTION,
	K_PROPERTY,
	K_TYPEDEF,
	K_STRUCT,
  K_CLASS,
	K_ENUM,
	K_MACRO,
} swiftKind;

static kindDefinition SwiftKinds[] = {
	{true, 'P', "protocol", "Protocol"},
	{true, 'm', "method", "Instance method"},
	{true, 'S', "static_method", "Static method"},
	{true, 'v', "var", "Global variable"},
	{true, 'E', "field", "Type field"},
	{true, 'f', "function", "A function"},
	{true, 'p', "property", "A property"},
	{true, 't', "typedef", "A type alias"},
	{true, 's', "struct", "A type structure"},
	{true, 'c', "class", "A type class"},
	{true, 'e', "enum", "An enumeration"},
	{true, 'M', "macro", "A preprocessor macro"},
};

typedef enum {
  SwiftAssociatedType,
  SwiftClass,
  SwiftDeinit,
  SwiftEnum,
  SwiftExtension,
  SwiftFilePrivate,
  SwiftFunc,
  SwiftImport,
  SwiftInit,
  SwiftInOut,
  SwiftInternal,
  SwiftLet,
  SwiftOpen,
  SwiftOperator,
  SwiftPrivate,
  SwiftProtocol,
  SwiftPublic,
  SwiftReThrows,
  SwiftStatic,
  SwiftStruct,
  SwiftSubscript,
  SwiftTypeAlias,
  SwiftVar,
  SwiftReturns,
	SwiftIdentifier,

	Tok_COMA,	/* ',' */
	Tok_PLUS,	/* '+' */
	Tok_MINUS,	/* '-' */
	Tok_PARL,	/* '(' */
	Tok_PARR,	/* ')' */
	Tok_CurlL,	/* '{' */
	Tok_CurlR,	/* '}' */
	Tok_SQUAREL,	/* '[' */
	Tok_SQUARER,	/* ']' */
	Tok_semi,	/* ';' */
	Tok_dpoint,	/* ':' */
  Tok_EQUAL, /* '=' */
  Tok_Sharp, /* '#' */
	Tok_Backslash,	/* '\\' */
	Tok_Asterisk,	/* '*' */
	Tok_ANGLEL,		/* '<' */
	Tok_ANGLER,		/* '>' */
	Tok_EOL,	/* '\r''\n' */
	Tok_STRING,	/* "..." */
	Tok_any,

	Tok_EOF	/* END of file */
} swiftKeyword;

typedef swiftKeyword swiftToken;


static langType Lang_Swift;
static swiftKind parentType;

static void swiftInitialize (const langType language)
{
	Lang_Swift = language;
}

static fieldDefinition SwiftFields [] = {
	{
		.name = "extension",
		.description = "extension attached to the class",
		.enabled = true,
	},
	{
		.name = "protocols",
		.description = "protocols that the class (or extension) conforms to",
		.enabled = true,
	},
};



static const keywordTable swiftKeywordTable[] = {
{"associatedtype", SwiftAssociatedType},
{"class", SwiftClass},
{"deinit", SwiftDeinit},
{"enum", SwiftEnum},
{"extension", SwiftExtension},
{"fileprivate", SwiftFilePrivate},
{"func", SwiftFunc},
{"import", SwiftImport},
{"init", SwiftInit},
{"inout", SwiftInOut},
{"internal", SwiftInternal},
{"let", SwiftLet},
{"open", SwiftOpen},
{"operator", SwiftOperator},
{"private", SwiftPrivate},
{"protocol", SwiftProtocol},
{"public", SwiftPublic},
{"rethrows", SwiftReThrows},
{"static", SwiftStatic},
{"struct", SwiftStruct},
{"subscript", SwiftSubscript},
{"typealias", SwiftTypeAlias},
{"var", SwiftVar},
{"->", SwiftReturns}
};

/*//////////////////////////////////////////////////////////////////////
//// Parsing                                    */
typedef void (*parseNext) (vString * const ident, swiftToken what);

/********** Helpers */
/* This variable hold the 'parser' which is going to
 * handle the next token */
static parseNext toDoNext;

/* Special variable used by parser eater to
 * determine which action to put after their
 * job is finished. */
static parseNext comeAfter;


/********** Grammar */
static void globalScope (vString * const ident, swiftToken what);

/*//////////////////////////////////////////////////////////////////
//// lexingInit             */
typedef struct _lexingState {
	vString *name;	/* current parsed identifier/operator */
	const unsigned char *cp;	/* position in stream */
} lexingState;

/*//////////////////////////////////////////////////////////////////////
//// Lexing                                     */
static bool isNum (char c)
{
	return c >= '0' && c <= '9';
}

static bool isLowerAlpha (char c)
{
	return c >= 'a' && c <= 'z';
}

static bool isUpperAlpha (char c)
{
	return c >= 'A' && c <= 'Z';
}

static bool isAlpha (char c)
{
	return isLowerAlpha (c) || isUpperAlpha (c);
}

static bool isIdent (char c)
{
	return isNum (c) || isAlpha (c) || c == '_';
}

static bool isSpace (char c)
{
	return c == ' ' || c == '\t';
}

static void readIdentifier (lexingState * st)
{
	const unsigned char *p;
	vStringClear (st->name);

	/* first char is a simple letter */
	if (isAlpha (*st->cp) || *st->cp == '_')
		vStringPut (st->name, (int) *st->cp);

	/* Go till you get identifier chars */
	for (p = st->cp + 1; isIdent (*p); p++)
		vStringPut (st->name, (int) *p);

	st->cp = p;
}

/* read the @something attributes
 * We currently don't support creating tags for attributes */
static void readAttributes (lexingState * st)
{
	const unsigned char *p;
	vStringClear (st->name);

	/* first char is a simple letter */
	if (*st->cp == '@')
		vStringPut (st->name, (int) *st->cp);

	/* Go till you get identifier chars */
	for (p = st->cp + 1; isIdent (*p); p++)
		vStringPut (st->name, (int) *p);

	st->cp = p;
}

/* return true if it end with an end of line */
static void eatWhiteSpace (lexingState * st)
{
	const unsigned char *cp = st->cp;
	while (isSpace (*cp))
		cp++;

	st->cp = cp;
}

static void readString (lexingState * st)
{
	bool lastIsBackSlash = false;
	bool unfinished = true;
	const unsigned char *c = st->cp + 1;

	vStringClear (st->name);

	while (unfinished)
	{
		/* end of line should never happen.
		 * we tolerate it */
		if (c == NULL || c[0] == '\0')
			break;
		else if (*c == '"' && !lastIsBackSlash)
			unfinished = false;
		else
		{
			lastIsBackSlash = *c == '\\';
			vStringPut (st->name, (int) *c);
		}

		c++;
	}

	st->cp = c;
}

static void eatComment (lexingState * st)
{
	bool unfinished = true;
	bool lastIsStar = false;
	const unsigned char *c = st->cp + 2;

	while (unfinished)
	{
		/* we've reached the end of the line..
		 * so we have to reload a line... */
		if (c == NULL || *c == '\0')
		{
			st->cp = readLineFromInputFile ();
			/* WOOPS... no more input...
			 * we return, next lexing read
			 * will be null and ok */
			if (st->cp == NULL)
				return;
			c = st->cp;
		}
		/* we've reached the end of the comment */
		else if (*c == '/' && lastIsStar)
			unfinished = false;
		else
		{
			lastIsStar = '*' == *c;
			c++;
		}
	}

	st->cp = c;
}

/* The lexer is in charge of reading the file.
 * Some of sub-lexer (like eatComment) also read file.
 * lexing is finished when the lexer return Tok_EOF */
static swiftKeyword lex (lexingState * st)
{
	int retType;

	/* handling data input here */
	while (st->cp == NULL || st->cp[0] == '\0')
	{
		st->cp = readLineFromInputFile ();
		if (st->cp == NULL)
			return Tok_EOF;

		return Tok_EOL;
	}

  // Note: We don't support unicode-charaters just yet
	if (isAlpha (*st->cp) || (*st->cp == '_'))
	{
		readIdentifier (st);
		retType = lookupKeyword (vStringValue (st->name), Lang_Swift);

		if (retType == -1)	/* If it's not a keyword */
		{
			return SwiftIdentifier;
		}
		else
		{
			return retType;
		}
	}
	else if (*st->cp == '@')
	{
		readAttributes (st);
		retType = lookupKeyword (vStringValue (st->name), Lang_Swift);

		if (retType == -1)	/* If it's not a keyword */
		{
			return Tok_any;
		}
		else
		{
			return retType;
		}
	} else if (*st->cp == '-')
  {
	if (st->cp[1] == '>' && st->cp[2] == ' ')	/* It's the -> operator */
			{
        st->cp = st->cp + 3;
        return SwiftReturns;
      }
  }
	else if (isSpace (*st->cp))
	{
		eatWhiteSpace (st);
		return lex (st);
	}
	else
		switch (*st->cp)
		{
		case '(':
			st->cp++;
			return Tok_PARL;

		case '\\':
			st->cp++;
			return Tok_Backslash;

		case '#':
			st->cp++;
			return Tok_Sharp;

		case '/':
			if (st->cp[1] == '*')	/* ergl, a comment */
			{
				eatComment (st);
				return lex (st);
			}
			else if (st->cp[1] == '/')
			{
				st->cp = NULL;
				return lex (st);
			}
			else
			{
				st->cp++;
				return Tok_any;
			}
			break;

		case ')':
			st->cp++;
			return Tok_PARR;
		case '{':
			st->cp++;
			return Tok_CurlL;
		case '}':
			st->cp++;
			return Tok_CurlR;
		case '[':
			st->cp++;
			return Tok_SQUAREL;
		case ']':
			st->cp++;
			return Tok_SQUARER;
		case ',':
			st->cp++;
			return Tok_COMA;
		case ';':
			st->cp++;
			return Tok_semi;
		case ':':
			st->cp++;
			return Tok_dpoint;
		case '"':
			readString (st);
			return Tok_STRING;
		case '+':
			st->cp++;
			return Tok_PLUS;
		case '-':
			st->cp++;
			return Tok_MINUS;
		case '*':
			st->cp++;
			return Tok_Asterisk;
		case '<':
			st->cp++;
			return Tok_ANGLEL;
		case '>':
			st->cp++;
			return Tok_ANGLER;
    case '=':
      st->cp++;
      return Tok_EQUAL;
		default:
			st->cp++;
			break;
		}

	/* default return if nothing is recognized,
	 * shouldn't happen, but at least, it will
	 * be handled without destroying the parsing. */
	return Tok_any;
}

static vString *parentName = NULL;
static void pushEnclosingContext (const vString * parent, swiftKind type)
{
	vStringCopy (parentName, parent);
	parentType = type;
}

static unsigned int parentCorkIndex = CORK_NIL;
static void pushEnclosingContextFull (const vString * parent, swiftKind type, unsigned int corkIndex)
{
	pushEnclosingContext (parent, type);
	parentCorkIndex = corkIndex;
}

static void popEnclosingContext (void)
{
	vStringClear (parentName);
	parentCorkIndex = CORK_NIL;
}

static int ignoreBalanced_count = 0;
static void ignoreBalanced (vString * const ident CTAGS_ATTR_UNUSED, swiftToken what)
{

  switch (what)
  {
  case Tok_PARL:
  case Tok_CurlL:
  case Tok_SQUAREL:
    ignoreBalanced_count++;
    break;

  case Tok_PARR:
  case Tok_CurlR:
  case Tok_SQUARER:
    ignoreBalanced_count--;
    break;

  default:
    /* don't care */
    break;
  }

  if (ignoreBalanced_count == 0)
    toDoNext = comeAfter;
}


static void prepareTag (tagEntryInfo * tag, vString const *name, swiftKind kind)
{
	initTagEntry (tag, vStringValue (name), kind);

	if (vStringLength (parentName) > 0)
	{
		tag->extensionFields.scopeKindIndex = parentType;
		tag->extensionFields.scopeName = vStringValue (parentName);
    tag->extensionFields.scopeIndex = parentCorkIndex;
	}
}

static int addTag (vString * const ident, int kind)
{
	tagEntryInfo toCreate;

	prepareTag (&toCreate, ident, kind);
	return makeTagEntry (&toCreate);
}

static void waitUntilNextStatement (vString * const ident, swiftToken what) {

	static parseNext prevWaitUntilNextStmt = NULL;

	if (prevWaitUntilNextStmt != NULL)
	{
		comeAfter = prevWaitUntilNextStmt;
		prevWaitUntilNextStmt = NULL;
	}

  switch(what)
  {
    case SwiftFunc:
    case SwiftVar:
    case SwiftLet:
    case SwiftClass:
    case Tok_CurlR:
      toDoNext = comeAfter;
      comeAfter(ident, what);
      break;
    case Tok_CurlL:
      toDoNext = &ignoreBalanced;
      ignoreBalanced (ident, what);
      prevWaitUntilNextStmt = comeAfter;
      comeAfter = &waitUntilNextStatement;
      break;
    default:
      break;
  }

}

static void parseClass(vString * const ident, swiftToken what);
static void parseFunction(vString *const ident, swiftToken what);
static bool isStaticMethod = false;
static void parseClassOrClassMembers(vString *const ident, swiftToken what) {
  switch(what) {
    case SwiftFunc:
      isStaticMethod = true;
      toDoNext = &parseFunction;
      break;
    case SwiftIdentifier:
      parseClass(ident, what);
      break;
    default:
      break;
  }
}

static vString *fullMethodName;
static vString *prevIdent;
static void parseFunction(vString *const ident, swiftToken what)
{
  switch(what)
  {
    case SwiftInit:
    case SwiftDeinit:
    case SwiftIdentifier:
      if(vStringLength(fullMethodName) == 0)
      {
        vStringCopy(fullMethodName, ident);
      }
      else
      {
        vStringCopy(prevIdent, ident);
      }
      break;
    case Tok_dpoint:
      vStringCat (fullMethodName, prevIdent);
      vStringPut (fullMethodName, ':');
      vStringClear (prevIdent);
      break;
    case Tok_CurlL:
      if (isStaticMethod)
      {
        addTag(fullMethodName, K_STATIC_METHOD);
        isStaticMethod = false;
      } else
      {
        addTag(fullMethodName, K_METHOD);
      }
      vStringClear (fullMethodName);
      toDoNext = &ignoreBalanced;
      ignoreBalanced (ident, what);
    default:
      break;
  }
}

static vString *propertyName;
static void parseProperty(vString *const ident, swiftToken what)
{
  switch(what)
  {
    case SwiftIdentifier:
      if(vStringLength(propertyName) == 0)
      {
        vStringCopy(propertyName, ident);
      }
      break;
    case Tok_EQUAL:
    case Tok_dpoint:
      addTag( propertyName, K_PROPERTY);
      toDoNext = &waitUntilNextStatement;
      vStringClear(propertyName);
      break;
    default:
      break;
  }
}

static void parseClassMembers(vString *const ident, swiftToken what)
{
	static parseNext prevClass = NULL;

	if (prevClass != NULL)
	{
		comeAfter = prevClass;
		prevClass = NULL;
	}

  switch(what)
  {

    case SwiftInit:
    case SwiftDeinit:
      toDoNext = &parseFunction; /* init is just a glorified function */
      prevClass = comeAfter;
      comeAfter = &parseClassMembers;
      parseFunction(ident, what);
      break;
    case SwiftFunc:
      toDoNext = &parseFunction;
      prevClass = comeAfter;
      comeAfter = &parseClassMembers;
      break;
    case SwiftClass: /* it could be a class declaration or a class function */
      toDoNext = &parseClassOrClassMembers;
      prevClass = comeAfter;
      comeAfter = &parseClassMembers;
      break;
    case SwiftVar:
    case SwiftLet:
      toDoNext = &parseProperty;
      prevClass = comeAfter;
      comeAfter = &parseClassMembers;
      break;
    case Tok_CurlR:
      popEnclosingContext ();
      toDoNext = comeAfter;
      break;
    default:
      break;
  }
}


static void parseClass(vString * const ident, swiftToken what)
{
  if(what == SwiftIdentifier)
  {
        unsigned int idx = addTag ( ident, K_CLASS);
        pushEnclosingContextFull(ident, K_CLASS, idx);
        toDoNext = &parseClassMembers;
  }
}

static void parseStruct(vString * const ident, swiftToken what)
{
  if(what == SwiftIdentifier)
  {
        unsigned int idx = addTag ( ident, K_STRUCT);
        pushEnclosingContextFull(ident, K_STRUCT, idx);
        toDoNext = &parseClassMembers;
  }
}

static void globalScope (vString * const ident, swiftToken what)
{
  switch(what)
  {
    case SwiftClass:
      toDoNext = &parseClass;
      comeAfter = &globalScope;
      break;
    case SwiftFunc:
      toDoNext = &parseFunction;
      comeAfter = &globalScope;
      break;
    case SwiftStruct:
      toDoNext = &parseStruct;
      comeAfter = &globalScope;
      break;
    default:
      break;
  }
}

static void findSwiftTags (void)
{
	vString *name = vStringNew ();
	lexingState st;
	swiftToken tok;

	parentName = vStringNew ();
	fullMethodName = vStringNew ();
  propertyName = vStringNew ();
	prevIdent = vStringNew ();

	/* (Re-)initialize state variables, this might be a second file */
	comeAfter = NULL;
	ignoreBalanced_count = 0;

	st.name = vStringNew ();
	st.cp = readLineFromInputFile ();
	toDoNext = &globalScope;
	tok = lex (&st);
	while (tok != Tok_EOF)
	{
		(*toDoNext) (st.name, tok);
		tok = lex (&st);
	}
	vStringDelete(st.name);
  vStringDelete(prevIdent);
	vStringDelete (name);
	vStringDelete (parentName);
	vStringDelete (fullMethodName);
  vStringDelete (propertyName);
	parentName = NULL;
	fullMethodName = NULL;
  prevIdent = NULL;
  propertyName = NULL;
	parentCorkIndex = CORK_NIL;
}


extern parserDefinition *SwiftParser (void)
{
	static const char *const extensions[] = { "swift", NULL };
	parserDefinition *def = parserNew ("Swift");
	def->kindTable = SwiftKinds;
	def->kindCount = ARRAY_SIZE (SwiftKinds);
	def->extensions = extensions;
	def->fieldTable = SwiftFields;
	def->fieldCount = ARRAY_SIZE (SwiftFields);
	def->parser = findSwiftTags;
	def->initialize = swiftInitialize;
	def->keywordTable = swiftKeywordTable;
	def->keywordCount = ARRAY_SIZE (swiftKeywordTable);
	def->useCork = CORK_QUEUE;
	return def;
}
