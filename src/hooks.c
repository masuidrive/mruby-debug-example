#include <mruby.h>
#include <stdio.h>
#include <string.h>
#include "mruby/irep.h"
#include "mruby/string.h"
#include "mruby/proc.h"

#define MAXARG_Bx        ((1<<16)-1)
#define MAXARG_sBx       (MAXARG_Bx>>1)         /* `sBx' is signed */

#define GET_OPCODE(i) ((int)(((mrb_code)(i)) & 0x7f))
#define GETARG_A(i)   ((int)((((mrb_code)(i)) >> 23) & 0x1ff))
#define GETARG_B(i)   ((int)((((mrb_code)(i)) >> 14) & 0x1ff))
#define GETARG_C(i)   ((int)((((mrb_code)(i)) >>  7) & 0x7f))
#define GETARG_Bx(i)  ((int)((((mrb_code)(i)) >>  7) & 0xffff))
#define GETARG_sBx(i) ((int)(GETARG_Bx(i)-MAXARG_sBx))
#define GETARG_Ax(i)  ((int)((((mrb_code)(i)) >>  7) & 0x1ffffff))
#define GETARG_UNPACK_b(i,n1,n2) ((int)((((mrb_code)(i)) >> (7+n2)) & (((1<<n1)-1))))
#define GETARG_UNPACK_c(i,n1,n2) ((int)((((mrb_code)(i)) >> 7) & (((1<<n2)-1))))
#define GETARG_b(i)   GETARG_UNPACK_b(i,14,2)
#define GETARG_c(i)   GETARG_UNPACK_c(i,14,2)

#define OP_L_STRICT  1
#define OP_L_CAPTURE 2
#define OP_L_METHOD  OP_L_STRICT
#define OP_L_LAMBDA  (OP_L_STRICT|OP_L_CAPTURE)
#define OP_L_BLOCK   OP_L_CAPTURE

#define OP_R_NORMAL 0
#define OP_R_BREAK  1
#define OP_R_RETURN 2


enum {
OP_NOP=0,/*                                                             */
OP_MOVE,/*      A B     R(A) := R(B)                                    */
OP_LOADL,/*     A Bx    R(A) := Lit(Bx)                                 */
OP_LOADI,/*     A sBx   R(A) := sBx                                     */
OP_LOADSYM,/*   A Bx    R(A) := Sym(Bx)                                 */
OP_LOADNIL,/*   A       R(A) := nil                                     */
OP_LOADSELF,/*  A       R(A) := self                                    */
OP_LOADT,/*     A       R(A) := true                                    */
OP_LOADF,/*     A       R(A) := false                                   */

OP_GETGLOBAL,/* A Bx    R(A) := getglobal(Sym(Bx))                      */
OP_SETGLOBAL,/* A Bx    setglobal(Sym(Bx), R(A))                        */
OP_GETSPECIAL,/*A Bx    R(A) := Special[Bx]                             */
OP_SETSPECIAL,/*A Bx    Special[Bx] := R(A)                             */
OP_GETIV,/*     A Bx    R(A) := ivget(Sym(Bx))                          */
OP_SETIV,/*     A Bx    ivset(Sym(Bx),R(A))                             */
OP_GETCV,/*     A Bx    R(A) := cvget(Sym(Bx))                          */
OP_SETCV,/*     A Bx    cvset(Sym(Bx),R(A))                             */
OP_GETCONST,/*  A Bx    R(A) := constget(Sym(Bx))                       */
OP_SETCONST,/*  A Bx    constset(Sym(Bx),R(A))                          */
OP_GETMCNST,/*  A Bx    R(A) := R(A)::Sym(B)                            */
OP_SETMCNST,/*  A Bx    R(A+1)::Sym(B) := R(A)                          */
OP_GETUPVAR,/*  A B C   R(A) := uvget(B,C)                              */
OP_SETUPVAR,/*  A B C   uvset(B,C,R(A))                                 */

OP_JMP,/*       sBx     pc+=sBx                                         */
OP_JMPIF,/*     A sBx   if R(A) pc+=sBx                                 */
OP_JMPNOT,/*    A sBx   if !R(A) pc+=sBx                                */
OP_ONERR,/*     sBx     rescue_push(pc+sBx)                             */
OP_RESCUE,/*    A       clear(exc); R(A) := exception (ignore when A=0) */
OP_POPERR,/*    A       A.times{rescue_pop()}                           */
OP_RAISE,/*     A       raise(R(A))                                     */
OP_EPUSH,/*     Bx      ensure_push(SEQ[Bx])                            */
OP_EPOP,/*      A       A.times{ensure_pop().call}                      */

OP_SEND,/*      A B C   R(A) := call(R(A),mSym(B),R(A+1),...,R(A+C))    */
OP_SENDB,/*     A B C   R(A) := call(R(A),mSym(B),R(A+1),...,R(A+C),&R(A+C+1))*/
OP_FSEND,/*     A B C   R(A) := fcall(R(A),mSym(B),R(A+1),...,R(A+C-1)) */
OP_CALL,/*      A B C   R(A) := self.call(R(A),.., R(A+C))              */
OP_SUPER,/*     A B C   R(A) := super(R(A+1),... ,R(A+C-1))             */
OP_ARGARY,/*    A Bx    R(A) := argument array (16=6:1:5:4)             */
OP_ENTER,/*     Ax      arg setup according to flags (24=5:5:1:5:5:1:1) */
OP_KARG,/*      A B C   R(A) := kdict[mSym(B)]; if C kdict.rm(mSym(B))  */
OP_KDICT,/*     A C     R(A) := kdict                                   */

OP_RETURN,/*    A B     return R(A) (B=normal,in-block return/break)    */
OP_TAILCALL,/*  A B C   return call(R(A),mSym(B),*R(C))                 */
OP_BLKPUSH,/*   A Bx    R(A) := block (16=6:1:5:4)                      */

OP_ADD,/*       A B C   R(A) := R(A)+R(A+1) (mSyms[B]=:+,C=1)           */
OP_ADDI,/*      A B C   R(A) := R(A)+C (mSyms[B]=:+)                    */
OP_SUB,/*       A B C   R(A) := R(A)-R(A+1) (mSyms[B]=:-,C=1)           */
OP_SUBI,/*      A B C   R(A) := R(A)-C (mSyms[B]=:-)                    */
OP_MUL,/*       A B C   R(A) := R(A)*R(A+1) (mSyms[B]=:*,C=1)           */
OP_DIV,/*       A B C   R(A) := R(A)/R(A+1) (mSyms[B]=:/,C=1)           */
OP_EQ,/*        A B C   R(A) := R(A)==R(A+1) (mSyms[B]=:==,C=1)         */
OP_LT,/*        A B C   R(A) := R(A)<R(A+1)  (mSyms[B]=:<,C=1)          */
OP_LE,/*        A B C   R(A) := R(A)<=R(A+1) (mSyms[B]=:<=,C=1)         */
OP_GT,/*        A B C   R(A) := R(A)>R(A+1)  (mSyms[B]=:>,C=1)          */
OP_GE,/*        A B C   R(A) := R(A)>=R(A+1) (mSyms[B]=:>=,C=1)         */

OP_ARRAY,/*     A B C   R(A) := ary_new(R(B),R(B+1)..R(B+C))            */
OP_ARYCAT,/*    A B     ary_cat(R(A),R(B))                              */
OP_ARYPUSH,/*   A B     ary_push(R(A),R(B))                             */
OP_AREF,/*      A B C   R(A) := R(B)[C]                                 */
OP_ASET,/*      A B C   R(B)[C] := R(A)                                 */
OP_APOST,/*     A B C   *R(A),R(A+1)..R(A+C) := R(A)                    */

OP_STRING,/*    A Bx    R(A) := str_dup(Lit(Bx))                        */
OP_STRCAT,/*    A B     str_cat(R(A),R(B))                              */

OP_HASH,/*      A B C   R(A) := hash_new(R(B),R(B+1)..R(B+C))           */
OP_LAMBDA,/*    A Bz Cz R(A) := lambda(SEQ[Bz],Cm)                      */
OP_RANGE,/*     A B C   R(A) := range_new(R(B),R(B+1),C)                */

OP_OCLASS,/*    A       R(A) := ::Object                                */
OP_CLASS,/*     A B     R(A) := newclass(R(A),mSym(B),R(A+1))           */
OP_MODULE,/*    A B     R(A) := newmodule(R(A),mSym(B))                 */
OP_EXEC,/*      A Bx    R(A) := blockexec(R(A),SEQ[Bx])                 */
OP_METHOD,/*    A B     R(A).newmethod(mSym(B),R(A+1))                  */
OP_SCLASS,/*    A B     R(A) := R(B).singleton_class                    */
OP_TCLASS,/*    A       R(A) := target_class                            */

OP_DEBUG,/*     A       print R(A)                                      */
OP_STOP,/*              stop VM                                         */
OP_ERR,/*       Bx      raise RuntimeError with message Lit(Bx)         */

OP_RSVD1,/*             reserved instruction #1                         */
OP_RSVD2,/*             reserved instruction #2                         */
OP_RSVD3,/*             reserved instruction #3                         */
OP_RSVD4,/*             reserved instruction #4                         */
OP_RSVD5,/*             reserved instruction #5                         */
};

static
void dumpcode(struct mrb_state* mrb, mrb_irep *irep, mrb_code *pc) {
  mrb_code c = *pc;
  int i=0, n=0;
  switch (GET_OPCODE(c)) {
    case OP_NOP:
      printf("OP_NOP\n");
      break;
    case OP_MOVE:
      printf("OP_MOVE\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
      break;
    case OP_LOADL:
      printf("OP_LOADL\tR%d\tL(%d)\n", GETARG_A(c), GETARG_Bx(c));
      break;
    case OP_LOADI:
      printf("OP_LOADI\tR%d\t%d\n", GETARG_A(c), GETARG_sBx(c));
      break;
    case OP_LOADSYM:
      printf("OP_LOADSYM\tR%d\t:%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
      break;
    case OP_LOADNIL:
      printf("OP_LOADNIL\tR%d\n", GETARG_A(c));
      break;
    case OP_LOADSELF:
      printf("OP_LOADSELF\tR%d\n", GETARG_A(c));
      break;
    case OP_LOADT:
      printf("OP_LOADT\tR%d\n", GETARG_A(c));
      break;
    case OP_LOADF:
      printf("OP_LOADF\tR%d\n", GETARG_A(c));
      break;
    case OP_GETGLOBAL:
      printf("OP_GETGLOBAL\tR%d\t:%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
      break;
    case OP_SETGLOBAL:
      printf("OP_SETGLOBAL\t:%s\tR%d\n",
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
             GETARG_A(c));
      break;
    case OP_GETCONST:
      printf("OP_GETCONST\tR%d\t:%d\n", GETARG_A(c),
             irep->syms[GETARG_Bx(c)]);
             //mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
      break;
    case OP_SETCONST:
      printf("OP_SETCONST\t:%s\tR%d\n",
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
             GETARG_A(c));
      break;
    case OP_GETMCNST:
      printf("OP_GETMCNST\tR%d\tR%d::%s\n", GETARG_A(c), GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
      break;
    case OP_SETMCNST:
      printf("OP_SETMCNST\tR%d::%s\tR%d\n", GETARG_A(c)+1,
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
             GETARG_A(c));
      break;
    case OP_GETIV:
      printf("OP_GETIV\tR%d\t%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
      break;
    case OP_SETIV:
      printf("OP_SETIV\t%s\tR%d\n",
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
             GETARG_A(c));
      break;
    case OP_GETUPVAR:
      printf("OP_GETUPVAR\tR%d\t%d\t%d\n",
             GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;
    case OP_SETUPVAR:
    printf("OP_SETUPVAR\n");
      printf("OP_SETUPVAR\tR%d\t%d\t%d\n",
             GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;
    case OP_GETCV:
      printf("OP_GETCV\tR%d\t%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
      break;
    case OP_SETCV:
      printf("OP_SETCV\t%s\tR%d\n",
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
             GETARG_A(c));
      break;
    case OP_JMP:
      printf("OP_JMP\t\t%03d\n", i+GETARG_sBx(c));
      break;
    case OP_JMPIF:
      printf("OP_JMPIF\tR%d\t%03d\n", GETARG_A(c), i+GETARG_sBx(c));
      break;
    case OP_JMPNOT:
      printf("OP_JMPNOT\tR%d\t%03d\n", GETARG_A(c), i+GETARG_sBx(c));
      break;
    case OP_SEND:
      printf("OP_SEND\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_SENDB:
      printf("OP_SENDB\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_TAILCALL:
      printf("OP_TAILCALL\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_SUPER:
      printf("OP_SUPER\tR%d\t%d\n", GETARG_A(c),
             GETARG_C(c));
      break;
    case OP_ARGARY:
      printf("OP_ARGARY\tR%d\t%d:%d:%d:%d\n", GETARG_A(c),
             (GETARG_Bx(c)>>10)&0x3f,
             (GETARG_Bx(c)>>9)&0x1,
             (GETARG_Bx(c)>>4)&0x1f,
             (GETARG_Bx(c)>>0)&0xf);
      break;

    case OP_ENTER:
      printf("OP_ENTER\t%d:%d:%d:%d:%d:%d:%d\n",
             (GETARG_Ax(c)>>18)&0x1f,
             (GETARG_Ax(c)>>13)&0x1f,
             (GETARG_Ax(c)>>12)&0x1,
             (GETARG_Ax(c)>>7)&0x1f,
             (GETARG_Ax(c)>>2)&0x1f,
             (GETARG_Ax(c)>>1)&0x1,
             GETARG_Ax(c) & 0x1);
      break;
    case OP_RETURN:
      printf("OP_RETURN\tR%d", GETARG_A(c));
      switch (GETARG_B(c)) {
      case OP_R_NORMAL:
        printf("\n"); break;
      case OP_R_RETURN:
        printf("\treturn\n"); break;
      case OP_R_BREAK:
        printf("\tbreak\n"); break;
      default:
        printf("\tbroken\n"); break;
        break;
      }
      break;
    case OP_BLKPUSH:
      printf("OP_BLKPUSH\tR%d\t%d:%d:%d:%d\n", GETARG_A(c),
             (GETARG_Bx(c)>>10)&0x3f,
             (GETARG_Bx(c)>>9)&0x1,
             (GETARG_Bx(c)>>4)&0x1f,
             (GETARG_Bx(c)>>0)&0xf);
      break;

    case OP_LAMBDA:
      printf("OP_LAMBDA\tR%d\tI(%+d)\t%d\n", GETARG_A(c), GETARG_b(c), GETARG_c(c));
      break;
    case OP_RANGE:
      printf("OP_RANGE\tR%d\tR%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;
    case OP_METHOD:
      printf("OP_METHOD\tR%d\t:%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]));
      break;

    case OP_ADD:
      printf("OP_ADD\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_ADDI:
      printf("OP_ADDI\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_SUB:
      printf("OP_SUB\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_SUBI:
      printf("OP_SUBI\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_MUL:
      printf("OP_MUL\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_DIV:
      printf("OP_DIV\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_LT:
      printf("OP_LT\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_LE:
      printf("OP_LE\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_GT:
      printf("OP_GT\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_GE:
      printf("OP_GE\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_EQ:
      printf("OP_EQ\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;

    case OP_STOP:
      printf("OP_STOP\n");
      break;

    case OP_ARRAY:
      printf("OP_ARRAY\tR%d\tR%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;
    case OP_ARYCAT:
      printf("OP_ARYCAT\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
      break;
    case OP_ARYPUSH:
      printf("OP_ARYPUSH\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
      break;
    case OP_AREF:
      printf("OP_AREF\tR%d\tR%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;
    case OP_APOST:
      printf("OP_APOST\tR%d\t%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;
    case OP_STRING:
      {
  mrb_value s = irep->pool[GETARG_Bx(c)];
  
  s = mrb_str_dump(mrb, s);
  printf("OP_STRING\tR%d\t%s\n", GETARG_A(c), RSTRING_PTR(s));
      }
      break;
    case OP_STRCAT:
      printf("OP_STRCAT\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
      break;
    case OP_HASH:
      printf("OP_HASH\tR%d\tR%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;

    case OP_OCLASS:
      printf("OP_OCLASS\tR%d\n", GETARG_A(c));
      break;
    case OP_CLASS:
      printf("OP_CLASS\tR%d\t:%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]));
      break;
    case OP_MODULE:
      printf("OP_MODULE\tR%d\t:%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]));
      break;
    case OP_EXEC:
      printf("OP_EXEC\tR%d\tI(%d)\n", GETARG_A(c), n+GETARG_Bx(c));
      break;
    case OP_SCLASS:
      printf("OP_SCLASS\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
      break;
    case OP_TCLASS:
      printf("OP_TCLASS\tR%d\n", GETARG_A(c));
      break;
    case OP_ERR:
      printf("OP_ERR\tL(%d)\n", GETARG_Bx(c));
      break;
    case OP_EPUSH:
      printf("OP_EPUSH\t:I(%d)\n", n+GETARG_Bx(c));
      break;
    case OP_ONERR:
      printf("OP_ONERR\t%03d\n", i+GETARG_sBx(c));
      break;
    case OP_RESCUE:
      printf("OP_RESCUE\tR%d\n", GETARG_A(c));
      break;
    case OP_RAISE:
      printf("OP_RAISE\tR%d\n", GETARG_A(c));
      break;
    case OP_POPERR:
      printf("OP_POPERR\t%d\n", GETARG_A(c));
      break;
    case OP_EPOP:
      printf("OP_EPOP\t%d\n", GETARG_A(c));
      break;

    default:
      printf("OP_unknown %d\t%d\t%d\t%d\n", GET_OPCODE(c),
             GETARG_A(c), GETARG_B(c), GETARG_C(c));
      printf("OP_ERR==%d\n", OP_ERR);
      break;
  }
}


static 
void hook_vm_fetch_code(struct mrb_state* mrb, mrb_irep *irep, mrb_code *pc, mrb_value *regs) {
  printf("%p: ", pc);
  dumpcode(mrb, irep, pc);
}

static
void hook_mrb_read_irep(struct mrb_state* mrb, int ret, const char *bin) {
  printf("mrb_read_irep(%p, %d, %p)\n", mrb, ret, bin);
}

struct saved_hooks {
  mrb_state *mrb;
  void (*hook_vm_fetch_code)(struct mrb_state* mrb, struct RProc *proc, mrb_value self, mrb_code *pc);
};
static struct saved_hooks *saved_hooks = NULL;
static int mrb_count = 0;

static struct saved_hooks* get_saved_hooks(struct mrb_state* mrb) {
  int i;
  for(i = 0; i < mrb_count; ++i) {
    if(saved_hooks[i].mrb == mrb) {
      return &saved_hooks[i];
    }
  }
  return NULL;
}

void mrb_mruby_debug_example_gem_init(mrb_state* mrb) {
#ifdef ENABLE_DEBUG
  /* saved original hooks */
  ++mrb_count;
  saved_hooks = mrb_realloc(mrb, saved_hooks, sizeof(struct saved_hooks) * mrb_count);
  saved_hooks[mrb_count - 1].mrb = mrb;
  saved_hooks[mrb_count - 1].hook_vm_fetch_code = mrb->hook_vm_fetch_code;

  /* set hooks */
  mrb->hook_vm_fetch_code = hook_vm_fetch_code;
#endif
}

void mrb_mruby_debug_example_gem_final(mrb_state* mrb) {
#ifdef ENABLE_DEBUG
  int i;
  for(i = 0; i < mrb_count; ++i) {
    if(saved_hooks[i].mrb == mrb) {
      mrb->hook_vm_fetch_code = saved_hooks[i].hook_vm_fetch_code;
      --mrb_count;
      memcpy(&saved_hooks[i], &saved_hooks[i+1], (mrb_count - i));
      saved_hooks = mrb_realloc(mrb, saved_hooks, sizeof(struct saved_hooks) * mrb_count);
      return;
    }
  }
#endif
}
