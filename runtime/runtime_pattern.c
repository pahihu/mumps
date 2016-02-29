// File: mumps/runtime/runtime_pattern.c
//
// module runtime - pattern match. Based on the FreeMUMPS code

//   FreeMUMPS is not to be placed under the GNU General Public Licence.
//   Anybody who wants to produce a proprietary product with parts of
//   FreeMUMPS is free to do so: Either with FreeMUMPS as part of
//   application programs or with FreeMUMPS as the basis of a new
//   commercial MUMPS implementation. However, who passes FreeMUMPS
//   or products based on it, must inform the recipient of the
//   FreeMUMPS Copyright, and he has to provide the FreeMUMPS
//   source codes without additional charges.


#include <stdio.h>                              // always include
#include <stdlib.h>                             // these two
#include <sys/types.h>                          // for u_char def
#include <string.h>                             // for string ops
#include "mumps.h"                              // standard includes
#include "error.h"                              // errors
#include "init.h"                               // init prototypes
#include "proto.h"                              // standard prototypes
#include "compile.h"                            // for routine buffer stuff


#define NOT '\47'
#define DELIM 255
#define PATDEPTH 20
#define EOL 255
#define SP '\40'
#define NUL '\0'
#define DEL '\177'

short pattern(cstring *a, cstring *b);
short patmat(cstring *str, cstring *code);
void pminmax(cstring *str, int *min, int *max);

short patmat(cstring *str, cstring *code)
{
  u_char ch = '\0';
  int i = 0;
  u_char f ='\0';
  int j = 0;
  int group = 0;
  int x = 0;
  u_char buf[257] = {'\0'};
  cstring *tmp;
  tmp = (cstring *) buf;
    ch = code->buf[x];
    if ((ch > '9' || ch < '0') && (ch != '.')) {	// if not number or dot
	return -(ERRM10);				// so bug off with err
    }							// end if
    tmp->buf[0] = ch;					// save this char
    i = 1;				// index into string
    f = '1';				// 'previous' character
    if (ch == '.') j = 1;
    else j = 0;				// point flag
    group = 0;				// grouped pattern match
    while (++x < code->len) {		// while more to do
        ch = code->buf[x];
	if ((ch >= '0') && (ch <= '9')) { // while more in a string of num's
	    tmp->buf[i++] = ch;		// copy the number out
	    f = '1';			// prev char
	    continue;			// go to next char
	}				// end if
	if (ch == '.') {		// dot in pattern
	    if (j) {			// they have two dots
		return -(ERRM10);	// bug off with err
	    }				// end if dot found
	    j++;			// flag error
	    tmp->buf[i++] = ch;		// copy the dot
	    f = '1';			// prev char
	    continue;			// go to next char
	}				// end if
	j = 0;				// flag ok
	if (ch == NOT) {		// negation of pattern class ?
	    ch = code->buf[x+1];
	    if ((ch == '"') || (ch >= 'A' && ch <= 'Z') ||
	        (ch >= 'a' && ch <= 'z')) {
		tmp->buf[i++] = NOT;
	    } else
		ch = NOT;
	}
	if (ch == '"') {
	    if (f != '1' && f != 'A') {
		return -(ERRM10);
	    }
	    for (;;)
	    {
		tmp->buf[i++] = ch;
		ch = code->buf[++x];
		if ( x >= code->len) {
		    return -(ERRM10);
		}
		if ((ch == '"') || (ch == DELIM)) {	// to cater for quotes
		    if ((f = code->buf[x+1]) != '"') {	// inside brackets the
			ch = DELIM;			// || ch == DELIM was
			break;				// added
		    }
		    x++;
		}
	    }
	    tmp->buf[i++] = ch;
	    f = '"';
	    continue;
	}
	if (ch == '(') {
	    if (f != '1') {
		return -(ERRM10);
	    }
	    group++;
	    f = '(';
	    tmp->buf[i++] = ch;
	    continue;
	}
	if (group && (ch == ',' || ch == ')')) {
	    if ((f == '1') || (f == '(')) {
		return -(ERRM10);
	    }
	    if (ch == ',') {
		f = '(';
		tmp->buf[i++] = ch;
		continue;
	    }
	    if (ch == ')') {
		group--;
		tmp->buf[i++] = ch;
		continue;
	    }
	}
	if (ch >= 'A' && ch <= 'Z')
	    ch += 32;			/* lower case conversion */

	if (ch != 'c' &&
	    ch != 'n' &&
	    ch != 'p' &&
	    ch != 'a' &&
	    ch != 'l' &&
	    ch != 'u' &&
	    ch != 'e')
	    break;

	if ((f != '1') && (f != 'A')) {
	    return -(ERRM10);
	}
	if (j) {
	    ch -= 32;
	    j = 0;
	}
	tmp->buf[i++] = ch;
	f = 'A';

    }
    if ((f == '1') || group) {
	return -(ERRM10);
    }
    tmp->buf[i++] = DELIM;
    tmp->len = i;	// the value of 'i' here is the length of the tmp string

    return pattern (str, tmp);
}    

// ***************************************************************************
// ***************************************************************************
void pminmax (str, min, max)   // auxiliary function for grouped pattern match 

        cstring   *str;                         // a pattern 'str'
        int       *min,                         // the minimum possible length
                  *max;                         // the maximum possible length

{   int     mininc,                             //
            maxinc,                             //
            i,                                  //
            ch;                                 //

    *min = 0;                                   // init
    *max = 0;                                   // all
    mininc = 0;                                 // these
    maxinc = 0;                                 // variables
    i = 0;                                      // to
    ch = 0;                                     // zero
    while (i < str->len-1) {                    // while more chars to do
        if (str->buf[i] != '.') {               // scan minimum repeat count
            ch = (str->buf[i++]) - '0';         // convert to int
            while (str->buf[i] >= '0' && str->buf[i] <= '9') { // a valid num
                ch *= 10;                       // decimal left shift (* by 10)
                ch += (str->buf[i++]) - '0';    // add the value ie. ch = 1,
            }                                   // value = 2, ch becomes 1*10+2
            mininc = ch;                        // init these
            maxinc = ch;                        // two variables
        } else {                                // no minimum specified
            mininc = 0;                         // use the defaults
            maxinc = 255;                       // for any number to start with
        }                                       // then
        if (str->buf[i] == '.') {               // scan maximum repeat count
            i++;                                // get next char in str
            if (str->buf[i] >= '0' && str->buf[i] <= '9') { // a valid number
                ch = (str->buf[i++]) - '0';     // convert to int
                while (str->buf[i] >= '0' && str->buf[i] <= '9') { // valid num
                    ch *= 10;                   // decimal left shift (* by 10)
                    ch += (str->buf[i++]) - '0'; // add the value ie. ch = 1
                }                               // value = 2, ch becomes 1*10+2
            } else                              // no "." found
                ch = 255;                       // use default maximum
            maxinc = ch;                        // set maximum
        }
                                                // skip pattern codes
        if (str->buf[i] == '"') {               // quoted string
            int     cnt;                        // need a new variable

            cnt = 0;                            // init counter
            while (str->buf[++i] != DELIM)      // while more to do
                cnt++;                          // increment couter
            mininc = mininc * cnt;              // set this minimum
            maxinc = maxinc * cnt;              // set this maximum
        }                                       //
        if (str->buf[i] == '"') {
            while (str->buf[++i] != DELIM) ;    //
            i++;                                //
        } else if (str->buf[i] == '(') {        //
            u_char    temp[257];                //
            cstring *tmp;			//
            u_char   *tcur;                     //
            int     tmin,                       //
                    tmax,                       //
                    Tmin,                       //
                    Tmax;                       //
            short   i1;                         //

            tmp = (cstring *) temp;             //
            tmin = 255;                         //
            tmax = 0;                           //
          alternation:;                         //
            i1 = 1;                             //
            tcur = tmp->buf;                    //
            while (i1) {                        //
                ch = str->buf[++i];             //
                *tcur++ = ch;                   //
                if (ch == '"')                  //
                    while ((*tcur++ = str->buf[++i]) != DELIM) ; //
                if (ch == '(')                  //
                    i1++;                       //
                if (ch == ')')                  //
                    i1--;                       //
                if (ch == ',' && i1 == 1)       //
                    i1--;                       //
            }                                   //
            *(--tcur) = DELIM;                  //
            tmp->len = tcur - tmp->buf + 1;	//
            pminmax (tmp, &Tmin, &Tmax);        //
            if (Tmin < tmin)                    //
                tmin = Tmin;                    //
            if (Tmax > tmax)                    //
                tmax = Tmax;                    //
            if (str->buf[i] == ',')             //
                goto alternation;               //
            i++;                                //
            mininc *= tmin;                     //
            maxinc *= tmax;                     //
        } else                                  //
            while (str->buf[++i] >= 'A') ;      //
        *min += mininc;                         //
        *max += maxinc;                         //
    }                                           //
    if (*max > 255)                             //
        *max = 255;                             //
    return;                                     //
}                                               // end pminmax()



// ***************************************************************************

short pattern (cstring *a, cstring *b)		// evaluates a ? b 
{   short   levels;                     	// depth of stack 
    int	    patx;                      		// match stack pointer 
    short   notpatclass;                	// pattern class negation 

    short   ptrpcd[PATDEPTH],
            position[PATDEPTH];

    short   mincnt[PATDEPTH],           	// minimum number of matches 
            maxcnt[PATDEPTH],           	// maximum number of matches 
            actcnt[PATDEPTH];           	// actual count of matches 
    short   Pflag,
            Pchar;                      	// status in alternation 
    short   altc;                       	// alternation counter 
    short   altcnt[PATDEPTH];           	// gr.pat.alternation counters 

    unsigned char gpmin[PATDEPTH][PATDEPTH][255]; // grpd pattern min length's 

    short   gp_position[PATDEPTH][PATDEPTH];    // grpd patt.pos.of substr

    u_char   *ptrtom;                     	// pointer to match code 
    u_char    patcode;				//
    int     ch;					//
    int     i;					//
    int     x;					//
    int     y;					//


    notpatclass = FALSE;                	// pattern class negation 
    patx = 0;
    x = 0;
    while (x < b->len-1) {                 	// get minimum repeat count 
        mincnt[patx] = 0;               	// default to 0
        maxcnt[patx] = 255;             	// default to 255
        altcnt[patx] = (-1);            	//
        if (b->buf[x] != '.') {                 // no "." to start
            ch = (b->buf[x++]) - '0';           // convert to int
            while (b->buf[x] >= '0' && b->buf[x] <= '9') { // more valid nums
                ch *= 10;               	// decimal shift (times by 10)
                ch += (b->buf[x++]) - '0';      // add the current value
            }                           	// end while more numbers
            mincnt[patx] = ch;          	// alter min repeat count
            if (b->buf[x] != '.')               // if still no dot
                maxcnt[patx] = ch;      	// max repeat is also set
        }                               	// end if no "." to start
                                        	// get maximum repeat count 
        if (b->buf[x] == '.') {                 // we have found a dot
            x++;                        	// go to next char in str
            if (b->buf[x] >= '0' && b->buf[x] <= '9') { // if a valid number
                ch = (b->buf[x++]) - '0';       // convert it to INT
                while (b->buf[x] >= '0' && b->buf[x] <= '9') { // more vld nums
                    ch *= 10;           	// decimal shift (times by 10)
                    ch += (b->buf[x++]) - '0';  // add the current value
                }                       	// end while more numbers
                maxcnt[patx] = ch;      	// set the max repeat count
            }                           	// end if valid number found
        }                               	// end if "." found

        if (maxcnt[patx] < mincnt[patx]) { 	// if it got screwed up
            return -(ERRM10);                 	// just impossible! 
        }
 
        ptrpcd[patx] = x;               	// save pos of this pat atom
        actcnt[patx] = 0;               	// init match count for expr
        position[patx] = 0;             	// pos of matching string
						// scan string, ignore if empty 

        if (b->buf[x] == '"' ||
           (b->buf[x] == '\'' && b->buf[x+1] == '"')) {	//
            if (b->buf[++x] == DELIM) {		// when at end of this literal
                x++;				// move along one pos
                continue;			// back to start
            }					//
            while (b->buf[++x] != DELIM) ;	// while more chars in literal
            x++;				// move along one pos
        } else if (b->buf[x] == '(') {		// not literal, but bracketed
            i = 1;				// init
            x++;				// increment
            while (x < b->len) {		// while more to look at
                ch = b->buf[x];			// look at this char
                x++;				// increment
                if (x == b->len) continue;	// don't increment if at end

                if (ch == '"') {		// literal inside brackets
                    while (b->buf[++x] != DELIM) ;	// go to end of literal
                }				//
                if (ch == '(') {		// nested brackets
                    i++;			// increment nest counter
                    continue;			// back to start
                }				//
                if (ch == ')') {		// a close bracket
                    i--;			// decrement nest counter
                    if (i < 1)			// not good if i drops below 1
                        break;			// bug out
                }				//
            }					//
        } else
          { while ((b->buf[++x] >= 'A') && (b->buf[x] <= 'z')) ; //replace code
          }

        if (++patx >= (PATDEPTH - 1)) {
            return -(ERRM10);                 // stack overflow 
        }
    }


// ********    LOGICAL BREAK IN THE CODE   *************
// ******** THIS NOW EVALUATES THE STRINGS *************


    levels = patx;
    if (b->buf[x - 1] == 'e' &&
        mincnt[levels - 1] == 0 &&
        maxcnt[levels - 1] == 255)
         b->buf[x - 1] = '~';         // frequent spec case: last patt is '.E' 
    mincnt[levels] = maxcnt[levels] = 1;        // sentinel, does never match 
    actcnt[levels] = 0;
    ptrpcd[levels] = x;                 // (*b==EOL) 

    patx = 0;
    y = 0;
    while (patx <= levels) {
        while (actcnt[patx] < mincnt[patx]) {
            actcnt[patx]++;
            if (y >= a->len) {
                if (patx >= levels) {
                    return 1; // match
                }
                if (patx > 0) {
                    if (actcnt[patx - 1] != maxcnt[patx - 1]) {
                        return 0; // no match
                    }
// after alternation we are not sure about 
// that supplement character               
                    if (b->buf[ptrpcd[patx - 1]] == '(') {
                        return 0;
                    }
                }
                if (b->buf[ptrpcd[patx]] == '"')

                return 0;
            }

            for (;;)
            {
                ptrtom = &b->buf[ptrpcd[patx]];
                ch = a->buf[y];
                for (;;)
                {
                    patcode = (*ptrtom++);              // first char
                    if ((notpatclass = (patcode == '\''))) // is it a negation
                        patcode = (*ptrtom++);          // yes -> next char
                    switch (patcode) {                  // we live in an 
                                                        // ASCII/ISO world !! 
                    case 'c':                           // control char
                        if (((ch < SP && ch >= NUL)     // between space & Null
                             || ch == DEL) != notpatclass) // or delete
                            goto match;                 // and not a negation
                        break;
                    case 'n':                           // numeric
                        if ((ch <= '9' && ch >= '0') != notpatclass) // 0 <-> 9
                            goto match;                 // and not a negation
                        break;
                    case 'p':                                   // punctuation
                        if (((ch >= SP && ch <= '/') ||         // if it
                             (ch >= ':' && ch <= '@') ||        // is in
                             (ch >= '[' && ch <= '`') ||        // any of
                             (ch >= '{' && ch <= '~') ||        // these ranges
                             (ch == '\200')) != notpatclass)    // and not
                            goto match;                         // a negation
                        break;
                    case 'a':                                   // alpha
                        if (((ch >= 'A' && ch <= 'Z') ||        // A-Z or a-z
                             (ch >= 'a' && ch <= 'z')) != notpatclass) // and
                            goto match;                         // not negation
                        break;
                    case 'l':                                   // lower case
                        if ((ch >= 'a' && ch <= 'z') != notpatclass) // a-z and
                            goto match;                         // not negation
                        break;
                    case 'u':                                   // upper case
                        if ((ch >= 'A' && ch <= 'Z') != notpatclass) // A-Z and
                            goto match;                         // not negation
                        break;
                    case 'e':                                   // any char and
//                        if (!notpatclass)                       // not negation
                        if ((ch > 0) != notpatclass)                       // not negation
                            goto match;
                        break;
                    case '"':                           // literal
                        i = 0;                          // pos in str
                        while (a->buf[y + i++] == (*ptrtom++)) ;
                        				// look until mismatch
                        if ((*--ptrtom) == DELIM) {     // mismatch was DELIM
                            if (notpatclass)            // if negation
                                goto nomatch;           // failed to match
                            x = ptrpcd[patx] + 1;	 // i dont know
                            while (b->buf[x++] != DELIM) // what to do!
                                y++;
                            goto match0;
                        }
                        if (notpatclass) {              // if a negation
                            i--;                        // go back one posi
                            while (*ptrtom++ != DELIM) { // until DELIM in atom
                                if (a->buf[y + i++] == EOL) // if it hits EOL
                                    goto nomatch;       // its no good
                            }                           // while not DELIM
                            x = ptrpcd[patx] + 2;       // at DELIM already
                            while (b->buf[x++] != DELIM) // while patx not at
                                y++;                    // DELIM, move along
                            goto match0;                // its ok
                        }                               // end if a negation
                        if (a->buf[y + i - 1] == EOL) { // we have reached EOL
                        }                               // end if reached EOL
                        goto nomatch;                   // bad luck, not match

                    case '~':{                          // '.E' as last atom
                            return 1;                 // its ok
                        }
                    case '(':{                          // grouped patt match
                            u_char    aaa[256];            // more chars
                            cstring *aa;
                            u_char    bbb[256];            // more chars
                            cstring *bb;
                            short   i1;                 // counter
                            int     min,                // minimums
                                    max;                // maximums
                            short   pflag;              // pattern flag
                            short   pchar;              // pattern char

                            aa = (cstring *) aaa;
                            bb = (cstring *) bbb;
                            
                            if (Pflag) {                // if set
                                pflag = Pflag;          // save values
                                pchar = Pchar;          // of both
                            } else {                    // otherwise
                                pflag = FALSE;          // set
                                pchar = EOL;            // these
                            }
                            if (altcnt[patx] < 0) {
                                for (altc = 0; altc < PATDEPTH; altc++)
                                    gpmin[patx][altc][1] = 0;
                            }
                            altcnt[patx] = 0;
                          alternation:;
                            i = 0;
                            i1 = 1;
                            while (i1) {
                                bb->buf[i] = *ptrtom++;
                                if (bb->buf[i] == '"') {
                                  while ((bb->buf[++i]=(*ptrtom++)) != DELIM) ;
                                }
                                if (bb->buf[i] == '(')
                                    i1++;
                                if (bb->buf[i] == ')')
                                    i1--;
                                if (bb->buf[i] == ',' && i1 == 1)
                                    i1--;
                                i++;
                            }
                            bb->len = --i;
                            pminmax (bb, &min, &max);

                            if ((i1 = gpmin[patx][altcnt[patx]][actcnt[patx]])
                                < min)
                                gpmin[patx][altcnt[patx]][actcnt[patx]]
                                = i1 
                                = min;
                            gpmin[patx][altcnt[patx]][actcnt[patx] + 1] = 0;

                            if (i1 > max) {
                                if (*(ptrtom - 1) == ',') {
                                    altcnt[patx]++;
                                    goto alternation;
                                }
                                goto nomatch;
                            }
// if number of chars too small, try anyway 
// to get info for "incomplete" match

			    for ( i = y; i < i1+y; i++)
			    { if (i >= a->len)
                                break;
                              else
                                aa->buf[i-y] = a->buf[i];
                            }
                            gp_position[patx][actcnt[patx]] = y; //chg i to y

                            for (;;)
                            {
                                aa->len = i-y;
                                i1 = patmat (aa, bb);
                                
                                if (i1 == 1) {
                                 gpmin[patx][altcnt[patx]][actcnt[patx]] = i-y;
                                 y += i-y;
                                 goto match0;
                                }
                                if (i1 != 0)
                                    return i1;
				if (i >= a->len) {
                                    Pflag = pflag;
                                    Pchar = pchar;
                                    if (*(ptrtom - 1) == ',') {
                                        altcnt[patx]++;
                                        goto alternation;
                                    }
                                    return 0;
                                }
                                aa->buf[i-y] = a->buf[i];
                                i++;
                            }
                        }
                    default:
                        goto nomatch;
                    }                   // end_switch 
                }                       // end repeat 


// ************* THINGS TO DO IF MATCH AND NO MATCH ******************

              nomatch:;
                if (patcode == '(') {
                    for (altc = 0; altc <= altcnt[patx]; altc++)
                        gpmin[patx][altc][actcnt[patx]] = 0;
                    if (--actcnt[patx] > 0) {
                        for (altc = 0; altc <= altcnt[patx]; altc++)
                            gpmin[patx][altc][actcnt[patx]]++;
                        y = gp_position[patx][actcnt[patx]]; // try prv pat's 
                        continue;
                    }
                }
                do {
                    actcnt[patx] = 0;
                    if (--patx < 0) {			// stack exhaust
                       return 0;
                    }
                    if (b->buf[ptrpcd[patx]] == '(') {
                        if (actcnt[patx] >= maxcnt[patx]) {
                            ++actcnt[patx];
                            patcode = '(';
                            goto nomatch;
                        }
                    }
                } while (++actcnt[patx] > maxcnt[patx]);
                y = position[patx];     // try previous patterns again 

            }                           // end repeat 
          match:;                       // match
            y++;                        // move pointer to next char
          match0:;                      // no action
        }
        position[patx++] = y;           // pos after last match 
    }        

    return 0;
}                                       // end of pattern 
// ****************************************************************************

