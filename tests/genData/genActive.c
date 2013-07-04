
typedef int __ssize_t;
typedef __ssize_t ssize_t;
typedef struct _sfio_s Sfio_t;
struct _sfio_s {
  unsigned char *next;
  unsigned char *endw;
  unsigned char *endr;
  unsigned char *endb;
  Sfio_t *push;
  unsigned short flags;
  short file;
  unsigned char *data;
  ssize_t size;
  ssize_t val;
};
extern ssize_t _Sfi;
extern Sfio_t *sfstdin;
extern Sfio_t *sfstdout;
extern Sfio_t *sfstderr;
extern Sfio_t _Sfstdin;
extern Sfio_t _Sfstdout;
extern Sfio_t _Sfstderr;
extern int errno;
typedef unsigned char uint8;
typedef int int32;
typedef struct stream_s *HRSstream_t;
struct t136 {
  int begin;
  int end;
  char *description;
};
typedef struct t136 *HRSslice_t;
extern void HRSstreamStart (HRSstream_t,void *,int,HRSslice_t);
extern int HRSstreamNext (HRSstream_t,char *);
typedef struct entry *HRSmap_t;
struct t141 {
  int low;
  int high;
};
typedef struct t141 range_desc_s;
struct t143 {
  int levels;
  range_desc_s *ranges;
};
typedef struct t143 HRSkey_desc_s;
struct t145 {
  int block;
  int line;
};
typedef struct t145 HRSkey_s;
typedef void (*HRSmapFunDefault_t) (HRSkey_s,char *);
struct t154 {
  char type;
  HRSmapFunDefault_t f;
  char *constant;
};
typedef struct t154 HRSmapDefault_s;
extern HRSmap_t HRSmapOpen (char *,int32,HRSmapDefault_s,int32,int32,HRSkey_desc_s,int32 (*) (uint8 *,uint8 *,int32),int32 (*) (uint8 *,int32,uint8 *));
extern void *HRSmapGet (HRSkey_s,HRSmap_t,void *);
extern void HRSmapPut (HRSkey_s,HRSmap_t,void *);
void HRSmapCopy (HRSmap_t,HRSmap_t);
HRSstream_t HRSmapToStream (HRSmap_t,HRSkey_s,HRSkey_s,int);
extern char *HRSprogramName;
extern void HRSinit (char *);
extern int optind;
extern char *optarg;
extern void __assert_fail (char const *,char const *,unsigned int,char const *);
struct t157 {
  short npa;
};
typedef struct t157 areacode_t;
struct t159 {
  short npa;
  short nxx;
};
typedef struct t159 exchange_t;
struct t161 {
  short npa;
  short nxx;
  int line;
};
typedef struct t161 line_t;
typedef int npanxx_r;
typedef int line_r;
struct t168 {
  npanxx_r primary;
  line_r secondary;
};
typedef line_t lpn_t;
typedef struct t168 ppn_t;
static lpn_t __H_4_user_coercion (ppn_t ppn)
{
  lpn_t lpn;
  lpn.npa = ((ppn.primary)/1000);
  lpn.nxx = ((ppn.primary)%1000);
  lpn.line = (ppn.secondary);
  return lpn;
}
static ppn_t __H_5_user_coercion (lpn_t lpn)
{
  ppn_t ppn;
  ppn.primary = (((lpn.npa)*1000)+(lpn.nxx));
  ppn.secondary = (lpn.line);
  return ppn;
}
typedef int date_t;
typedef int Htime_t;
struct t171 {
  lpn_t origin;
  lpn_t dialed;
  lpn_t terminated;
  date_t connecttime;
  Htime_t duration;
  char isTollFree;
  char isIntl;
  char isIncomplete;
  char isCIC;
};
typedef struct t171 HscampRec;
struct t183 {
  char call_f;
  char line_end_f;
  char nxx_end_f;
  char npa_end_f;
  char line_begin_f;
  char nxx_begin_f;
  char npa_begin_f;
  HscampRec call_v;
  lpn_t line_end_v;
  exchange_t nxx_end_v;
  areacode_t npa_end_v;
  lpn_t line_begin_v;
  exchange_t nxx_begin_v;
  areacode_t npa_begin_v;
};
typedef struct t183 line_e;
line_e beginLineDetect (lpn_t *prev,lpn_t *current)
{
  line_e __H_9_evt={0};
  line_e __H_8_evt={0};
  line_e __H_7_evt={0};
  line_e __H_6_evt={0};
  areacode_t a;
  exchange_t n;
  a.npa = ((*current).npa);
  n.npa = ((*current).npa);
  n.nxx = ((*current).nxx);
  if ((prev==((void *) 0))||(((*prev).npa)!=((*current).npa))) 
    return (((__H_6_evt.npa_begin_f = 1,__H_6_evt.npa_begin_v = a),((__H_6_evt.nxx_begin_f = 1,__H_6_evt.nxx_begin_v = n),(__H_6_evt.line_begin_f = 1,__H_6_evt.line_begin_v = (*current)))),__H_6_evt);
  if (((*prev).nxx)!=((*current).nxx)) 
    return (((__H_7_evt.nxx_begin_f = 1,__H_7_evt.nxx_begin_v = n),(__H_7_evt.line_begin_f = 1,__H_7_evt.line_begin_v = (*current))),__H_7_evt);
  if (((*prev).line)!=((*current).line)) 
    return ((__H_8_evt.line_begin_f = 1,__H_8_evt.line_begin_v = (*current)),__H_8_evt);
  return __H_9_evt;
}
line_e endLineDetect (lpn_t *current,lpn_t *next)
{
  line_e __H_13_evt={0};
  line_e __H_12_evt={0};
  line_e __H_11_evt={0};
  line_e __H_10_evt={0};
  areacode_t a;
  exchange_t n;
  (void) ((current!=((void *) 0)) ? 0 : (__assert_fail ("current != ((void *)0)","/home/fsmith/hancock/compiler/port/linux/hancock/tests/genData/../scampRec.hh",198,(char const *) 0),0));
  a.npa = ((*current).npa);
  n.nxx = ((*current).nxx);
  n.npa = ((*current).npa);
  if ((next==((void *) 0))||(((*current).npa)!=((*next).npa))) 
    return (((__H_10_evt.line_end_f = 1,__H_10_evt.line_end_v = (*current)),((__H_10_evt.npa_end_f = 1,__H_10_evt.npa_end_v = a),(__H_10_evt.nxx_end_f = 1,__H_10_evt.nxx_end_v = n))),__H_10_evt);
  if (((*current).nxx)!=((*next).nxx)) 
    return (((__H_11_evt.line_end_f = 1,__H_11_evt.line_end_v = (*current)),(__H_11_evt.nxx_end_f = 1,__H_11_evt.nxx_end_v = n)),__H_11_evt);
  if (((*current).line)!=((*next).line)) 
    return ((__H_12_evt.line_end_f = 1,__H_12_evt.line_end_v = (*current)),__H_12_evt);
  return __H_13_evt;
}
static line_e internalLineDetect (lpn_t *prev,lpn_t *curr,lpn_t *next)
{
  line_e __H_15_rhs;
  line_e __H_14_result;
  line_e b;
  line_e e;
  b = beginLineDetect (prev,curr);
  e = endLineDetect (curr,next);
  return (__H_14_result = b,(__H_15_rhs = e,((__H_15_rhs.npa_begin_f) ? (__H_14_result.npa_begin_f = 1,(__H_14_result.npa_begin_v = (__H_15_rhs.npa_begin_v),__H_14_result)) : __H_14_result,((__H_15_rhs.nxx_begin_f) ? (__H_14_result.nxx_begin_f = 1,(__H_14_result.nxx_begin_v = (__H_15_rhs.nxx_begin_v),__H_14_result)) : __H_14_result,((__H_15_rhs.line_begin_f) ? (__H_14_result.line_begin_f = 1,(__H_14_result.line_begin_v = (__H_15_rhs.line_begin_v),__H_14_result)) : __H_14_result,((__H_15_rhs.npa_end_f) ? (__H_14_result.npa_end_f = 1,(__H_14_result.npa_end_v = (__H_15_rhs.npa_end_v),__H_14_result)) : __H_14_result,((__H_15_rhs.nxx_end_f) ? (__H_14_result.nxx_end_f = 1,(__H_14_result.nxx_end_v = (__H_15_rhs.nxx_end_v),__H_14_result)) : __H_14_result,((__H_15_rhs.line_end_f) ? (__H_14_result.line_end_f = 1,(__H_14_result.line_end_v = (__H_15_rhs.line_end_v),__H_14_result)) : __H_14_result,((__H_15_rhs.call_f) ? (__H_14_result.call_f = 1,(__H_14_result.call_v = (__H_15_rhs.call_v),__H_14_result)) : __H_14_result,__H_14_result)))))))));
}
typedef lpn_t *__H_16_windowS[3];
line_e lineDetect (__H_16_windowS w)
{
  return internalLineDetect (w[0],w[1],w[2]);
}
typedef char aFApprox;
struct t204 {
  aFApprox inA;
  aFApprox out;
  aFApprox jumble;
};
typedef struct t204 aLApprox;
int Acompress_record (aLApprox *from,unsigned char *to_space,int ext_to_size)
{
  unsigned short v;
  v = ((((((*from).jumble)*(39+1))*(39+1))+(((*from).inA)*(39+1)))+((*from).out));
  to_space[1] = ((v>>8)&255);
  to_space[0] = (v&255);
  return 2;
}
int Auncompress_record (unsigned char *from,int from_size,aLApprox *to_space)
{
  unsigned short v;
  v = ((from[1]<<8)|from[0]);
  (*to_space).jumble = (v/((39+1)*(39+1)));
  v = (v%((39+1)*(39+1)));
  (*to_space).inA = (v/(39+1));
  (*to_space).out = (v%(39+1));
  return 2;
}
static range_desc_s __H_45_aMapranges[2]={{0,1000000-1},{0,10000-1}};
static HRSkey_desc_s __H_46_aMapkey={2,__H_45_aMapranges};
static aLApprox __H_44_aMap_default={39,39,39};
typedef HRSmap_t aMap;
extern int _stdprintf (char const *,...);
lpn_t build_pn (int npa,int nxx,int line)
{
  lpn_t temp;
  temp.npa = npa;
  temp.nxx = nxx;
  temp.line = line;
  return temp;
}
static int __H_49_transform (ppn_t *pr,lpn_t *lr)
{
  *lr = __H_4_user_coercion (*pr);
  return 1;
}
static int __H_50_wrapper (ppn_t *pr,lpn_t *lr)
{
  int isGood;
  int isWanted;
  *((int *) (&isWanted)) = 1;
  isGood = __H_49_transform (pr,lr);
  if (isGood) 
    {
    }
  return isGood&&isWanted;
}
void ch_map (aMap a)
{
  {
    int __H_51_eos;
    int __H_52_offset;
    __H_16_windowS __H_53_window={0,0,0};
    lpn_t __H_54_winValue[3];
    line_e __H_56_evt;
    HRSstream_t __H_57_stream;
    int __H_55_current;
    lpn_t __H_E_kline_begin_bm;
    aLApprox __H_E_kline_begin_bx;
    *((int *) (&__H_51_eos)) = 1;
    *((int *) (&__H_52_offset)) = 0;
    *((int *) (&__H_55_current)) = 1;
    {
      ppn_t __H_58_low_tmp;
      ppn_t __H_59_high_tmp;
      __H_58_low_tmp = __H_5_user_coercion (build_pn (0,0,0));
      __H_59_high_tmp = __H_5_user_coercion (build_pn (999,999,9999));
      __H_57_stream = HRSmapToStream (a,*((HRSkey_s *) (&__H_58_low_tmp)),*((HRSkey_s *) (&__H_59_high_tmp)),sizeof(lpn_t));
      HRSstreamStart (__H_57_stream,(void *) __H_50_wrapper,0,0);
    }
    while (__H_52_offset<2)
      {
        __H_51_eos = HRSstreamNext (__H_57_stream,(char *) (&__H_54_winValue[__H_55_current]));
        if (__H_51_eos==0) 
          break;
        __H_53_window[__H_52_offset+1] = (&__H_54_winValue[__H_55_current]);
        __H_52_offset++;
        __H_55_current = ((__H_55_current+1)%3);
      }
    while (__H_52_offset!=0)
      {
        __H_56_evt = lineDetect (__H_53_window);
        /******************Event blocks******************/
        if (__H_56_evt.line_begin_f) 
          {
            __H_E_kline_begin_bm = (__H_56_evt.line_begin_v);
            {
              {
              }
              {
                ppn_t __H_61_key;
                aLApprox __H_60_index;
                _stdprintf ("%03d %03d %04d: ",__H_E_kline_begin_bm.npa,__H_E_kline_begin_bm.nxx,__H_E_kline_begin_bm.line);
                __H_E_kline_begin_bx = ((__H_61_key = __H_5_user_coercion (__H_E_kline_begin_bm),HRSmapGet (*((HRSkey_s *) (&__H_61_key)),a,(void *) (&__H_60_index))),__H_60_index);
                _stdprintf ("%2d %2d %2d\n",__H_E_kline_begin_bx.inA,__H_E_kline_begin_bx.out,__H_E_kline_begin_bx.jumble);
              }
            }
          }
        /*************Finished event blocks**************/
        {
          __H_53_window[0] = __H_53_window[1];
          __H_53_window[1] = __H_53_window[2];
        }
        __H_51_eos = HRSstreamNext (__H_57_stream,(char *) (&__H_54_winValue[__H_55_current]));
        if (__H_51_eos==0) 
          {
            __H_52_offset--;
            __H_53_window[2] = 0;
          }
        else
          {
            __H_53_window[2] = (&__H_54_winValue[__H_55_current]);
            __H_55_current = ((__H_55_current+1)%3);
          }
      }
  }
}
int main (int argc,char **argv)
{
  HRSinit (argv[0]);
  ;
  {
    ppn_t __H_64_key;
    aMap a=0;
    aMap b=0;
    lpn_t l;
    aLApprox d={36,36,36};
    {
      HRSmapDefault_s __H_62_map_default;
      __H_62_map_default.type = 1;
      __H_62_map_default.constant = ((char *) (&__H_44_aMap_default));
      *((HRSmap_t *) (&a)) = HRSmapOpen ("data/activeMap",sizeof(aLApprox),__H_62_map_default,0,2,__H_46_aMapkey,(int32 (*) (uint8 *,uint8 *,int32)) Acompress_record,(int32 (*) (uint8 *,int32,uint8 *)) Auncompress_record);
    }
    {
      HRSmapDefault_s __H_63_map_default;
      __H_63_map_default.type = 1;
      __H_63_map_default.constant = ((char *) (&__H_44_aMap_default));
      *((HRSmap_t *) (&b)) = HRSmapOpen ("data/activeMapDC",sizeof(aLApprox),__H_63_map_default,0,2,__H_46_aMapkey,(int32 (*) (uint8 *,uint8 *,int32)) Acompress_record,(int32 (*) (uint8 *,int32,uint8 *)) Auncompress_record);
    }
    *((lpn_t *) (&l)) = build_pn (973,360,8077);
    (__H_64_key = __H_5_user_coercion (l),(HRSmapPut (*((HRSkey_s *) (&__H_64_key)),a,(void *) (&d)),d));
    HRSmapCopy (a,b);
    _stdprintf ("Checking activeMap.\n");
    ch_map (a);
    _stdprintf ("Checking activeMapDC.\n");
    ch_map (b);
  }
  return 0;
}
