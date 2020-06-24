CREATE OR REPLACE PROCEDURE PCX_CUST_APP_FP_LST(CUR_RESULT   OUT TYPES.CURSORTYPE, --结果游标
                                                I_PAGENUMBER IN NUMBER,   --页码
                                                I_PAGECOUNT  IN NUMBER,   --每页行数
                                                O_COUNT      OUT NUMBER,  --结果集总记录数
                                                I_SORT       IN VARCHAR2, --排序字段
                                                I_QUERY_TYPE IN NUMBER,   --查询类型(0|全部;1|视频双录;2|募集;3|存续;4|清算)
                                                I_CUST_NO    IN VARCHAR2, --客户号
                                                I_USR_ID     IN NUMBER,   --APP_ID
                                                I_FP_NM      IN VARCHAR2, --产品名称
                                                I_FP_BQ      IN NUMBER,   --产品标签 (1|最后抢筹;2|新品推荐;3|精品热销)
                                                I_FP_TYPE    IN NUMBER,   --产品类型 (1|浮动;0|固定)
                                                I_CPQX       IN NUMBER,   --产品期限 (1:0-3个月，2:3-6个月 3:6-12个月， 4:12-24个月 5：24个月以上)
                                                I_CPSYL      IN NUMBER,   --产品收益率 (1:3%-5%，2:5%-7% 3:7% -9%， 4:9%以上)
                                                I_TZLY       IN NUMBER     --投向行业
 ) IS
  /******************************************************************************
    项目名称：信托客户APP
    创建人员：WENGJIE
    创建日期：2018-03-26
    功能说明：
      按产品类型查询产品列表
    ---------------------------------------------------------------------------

    修改者        版本号        修改日期        说明
    WENGJIE       1.0.0         2018-03-26      创建
    WENGJIE       99.99         2018-07-08      持有中新增清算中份额
    WENGJIE       99.99         2018-08-15      国通改造
  ******************************************************************************/
  V_CUST_ID NUMBER(16);

  V_SQL     VARCHAR2(32767);
  V_SQL_MJ  VARCHAR2(32767);   --募集未录制列表
  V_SQL_CX  VARCHAR2(32767);   --存续未录制列表
  V_SORT    VARCHAR2(2000);
  V_COL_LST VARCHAR2(2000);
  V_ERR_MSG VARCHAR2(3000);
  V_WHERE   VARCHAR2(3000);
  V_USR_ID  NUMBER(16);
  V_KHH     NUMBER;
  V_QZKHID  NUMBER;
  V_PATH    VARCHAR2(600);
  V_TZLY    VARCHAR2(50);
BEGIN

  V_USR_ID:=I_USR_ID;
  IF I_CUST_NO LIKE '1%' THEN
     V_KHH:=I_CUST_NO-1000000000000;
     SELECT NVL(MAX(ID),-99) INTO V_QZKHID FROM TQZKHXX WHERE SSKHID=V_KHH;
  ELSE
     V_KHH:=I_CUST_NO-2000000000000;
  END IF;

    --0|首页——产品
  IF I_QUERY_TYPE = 0 THEN
    IF I_USR_ID IS NOT NULL THEN
      V_SQL := '
           SELECT * FROM
           (
             SELECT FP.ID   产品ID,
             FP.CPDM 产品代码,
             XM.CPMC_APP 产品名称,
             XM.CPMC_APP 产品全称,
             CASE WHEN (NVL(FP.SFJZ,0)=0 AND (SELECT COUNT(1) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )>1) THEN
                    (SELECT TRUNC(NVL(MIN(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )||''%-''||
                    (SELECT TRUNC(NVL(MAX(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )||''%''
                   WHEN (NVL(FP.SFJZ,0)=0 AND (SELECT COUNT(1) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )<=1) THEN
                     --(SELECT TRUNC(NVL(MAX(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )||''%''
                   (SELECT decode( TRUNC(NVL(MAX(JB.SYL), 0), 2),0,''浮动'',TRUNC(NVL(MAX(JB.SYL), 0), 2)|| ''%'') FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )

                WHEN NVL(FP.SFJZ,0)=1 THEN ''浮动''

               END  "业绩比较基准",
             (SELECT TRUNC(NVL(MIN(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID) 最低收益率,
             (SELECT TRUNC(NVL(MAX(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID) 最高收益率,
             CASE WHEN FP.CPQZLX=0 THEN 0
                  WHEN FP.CPQZLX=1 THEN ROUND(FP.CPQX/30)
                  WHEN FP.CPQZLX=3 THEN ROUND(FP.CPQX/12)
                  ELSE ROUND(FP.CPQX)
             END  产品期限,
             XM.GRRGQD/10000  起投金额,
             NVL(FP.SFJZ,0) 是否净值,
             (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) 产品类型,
             XM.CPBQ 产品标签,
             XM.SFSYZS 是否首页展示
             FROM TFP_CPDM FP,TXSXM XM
             WHERE FP.ID=XM.CPID AND XM.SJBZ=1
            and( xm.SJRQ < to_char(sysdate, ''YYYYMMDD'')
                      OR ((xm.SJRQ = to_char(sysdate, ''YYYYMMDD'') AND
                           XM.SJSJ <= TO_CHAR(SYSDATE, ''hh24:mi'')) OR
                           XM.SJSJ IS NULL)
                           )
                      AND (XM.XJRQ > TO_CHAR(SYSDATE, ''YYYYMMDD'')
                       OR (XM.XJRQ = TO_CHAR(SYSDATE, ''YYYYMMDD'') AND
                          (XM.XJSJ > TO_CHAR(SYSDATE, ''hh24:mi'') OR
                          XM.XJSJ IS NULL)))
          /* 20190409 YANGPEI 上架时间前：app不显示*/
            )';
    --游客查询首页产品
    ELSE
      V_SQL := 'select * from
           (SELECT FP.ID   产品ID,
           FP.CPDM 产品代码,
           XM.CPMC_APP 产品名称,
           XM.CPMC_APP 产品全称,
           CASE WHEN (NVL(FP.SFJZ,0)=0 AND (SELECT COUNT(1) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )>1) THEN
                    (SELECT TRUNC(NVL(MIN(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )||''%-''||
                    (SELECT TRUNC(NVL(MAX(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )||''%''
               -- WHEN (NVL(FP.SFJZ,0)=0 AND (SELECT COUNT(1) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)=0)>0)  THEN ''浮动'' /*新增收益率为0显示浮动  */
                WHEN (NVL(FP.SFJZ,0)=0 AND (SELECT COUNT(1) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )<=1) THEN
                     (SELECT  decode( TRUNC(NVL(MAX(JB.SYL), 0), 2),0,''浮动'',TRUNC(NVL(MAX(JB.SYL), 0), 2)|| ''%'') FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )
                WHEN NVL(FP.SFJZ,0)=1 THEN ''浮动''

               END  "业绩比较基准",
           (SELECT TRUNC(NVL(MIN(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID) 最低收益率,
           (SELECT TRUNC(NVL(MAX(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID) 最高收益率,
           CASE WHEN FP.CPQZLX=0 THEN 0
                  WHEN FP.CPQZLX=1 THEN ROUND(FP.CPQX/30)
                  WHEN FP.CPQZLX=3 THEN ROUND(FP.CPQX/12)
                  ELSE ROUND(FP.CPQX)
             END  产品期限,
           XM.GRRGQD/10000  起投金额,
           NVL(FP.SFJZ,0) 是否净值,
           (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) 产品类型,
           XM.CPBQ 产品标签,
           XM.SFSYZS 是否首页展示
           FROM TFP_CPDM FP,TXSXM XM
           WHERE FP.ID=XM.CPID AND XM.SJBZ=1
          and( xm.SJRQ < to_char(sysdate, ''YYYYMMDD'')
                      OR ((xm.SJRQ = to_char(sysdate, ''YYYYMMDD'') AND
                           XM.SJSJ <= TO_CHAR(SYSDATE, ''hh24:mi'')) OR
                           XM.SJSJ IS NULL)
                           )
                      AND (XM.XJRQ > TO_CHAR(SYSDATE, ''YYYYMMDD'')
                       OR (XM.XJRQ = TO_CHAR(SYSDATE, ''YYYYMMDD'') AND
                          (XM.XJSJ > TO_CHAR(SYSDATE, ''hh24:mi'') OR
                          XM.XJSJ IS NULL)))
       /* 20190409 YANGPEI 上架时间前：app不显示*/
       )';
    END IF;

    V_COL_LST := '产品ID, 产品代码, 产品名称, 产品全称, 业绩比较基准 , 起投金额 , 产品期限, 是否净值, 产品类型,最低收益率,产品期限   ';
    --全部产品筛选条件

    V_WHERE :=' WHERE 1=1 ';
    --产品筛选
    --1|浮动 0|固定
    IF I_FP_TYPE IS NOT NULL THEN
      V_WHERE := V_WHERE ||
                 ' AND 是否净值 = ' ||
                 I_FP_TYPE;
    END IF;

    --产品标签
    IF I_FP_BQ IS NOT NULL THEN
       V_WHERE := V_WHERE ||
                 ' AND 产品标签 = ' ||
                 I_FP_BQ;
    END IF;

    --投向行业
    IF I_TZLY IS NOT NULL THEN
       SELECT ''''||NOTE||'''' INTO V_TZLY FROM TXTDM ZD WHERE FLDM='TZLY' AND ZD.IBM=I_TZLY;
       V_WHERE := V_WHERE ||
                 ' AND 产品类型 = ' ||
                 V_TZLY;
    END IF;

    --产品名称筛选
    IF I_FP_NM IS NOT NULL THEN
      V_WHERE := V_WHERE || ' AND 产品全称 LIKE(''%' || I_FP_NM || '%'')';
    END IF;

    --产品期限筛选 (1:0-3个月，2:3-6个月 3:6-12个月， 4:12-24个月 5：24个月以上)
    IF I_CPQX IS NOT NULL THEN
      IF I_CPQX = 1 THEN
        V_WHERE := V_WHERE || ' AND 产品期限 > 0 AND 产品期限 <= 3 ';
      END IF;
      IF I_CPQX = 2 THEN
        V_WHERE := V_WHERE || ' AND 产品期限 > 3 AND 产品期限 <= 6 ';
      END IF;
      IF I_CPQX = 3 THEN
        V_WHERE := V_WHERE || ' AND 产品期限 > 6 AND 产品期限 <= 12 ';
      END IF;
      IF I_CPQX = 4 THEN
        V_WHERE := V_WHERE || ' AND 产品期限 > 12 AND 产品期限 <= 24';
      END IF;
      IF I_CPQX = 5 THEN
        V_WHERE := V_WHERE || ' AND 产品期限 > 24 ';
      END IF;
       V_WHERE := V_WHERE;
    END IF;

    --产品收益率筛选 ( 2:5%-7%  3:7% -9% 4:9%以上)
    IF I_CPSYL IS NOT NULL THEN
      IF I_CPSYL = 2 THEN
        V_WHERE := V_WHERE || ' AND 最低收益率 > 5 AND 最低收益率 <=7 ';
      END IF;
      IF I_CPSYL = 3 THEN
        V_WHERE := V_WHERE || ' AND 最低收益率 > 7 AND 最低收益率 <=9 ';
      END IF;
      IF I_CPSYL = 4 THEN
        V_WHERE := V_WHERE || ' AND 最低收益率 > 9 ';
      END IF;
     V_WHERE := V_WHERE;
    END IF;


    --排序 1,2|综合排序倒正; 3,4|收益率倒正; 5,6|期限倒正
    IF I_SORT IS NOT NULL THEN
      IF I_SORT = 1 THEN
        V_SORT := '起投金额 DESC,业绩比较基准 DESC ,产品ID desc NULLS LAST ';
      END IF;
      IF I_SORT = 2 THEN
        V_SORT := '起投金额 ASC,业绩比较基准 ASC ,产品ID desc NULLS LAST ';
      END IF;
      IF I_SORT = 3 THEN
        V_SORT := '最高收益率 DESC ,产品ID desc NULLS LAST ';
      END IF;
      IF I_SORT = 4 THEN
        V_SORT := '最高收益率  ASC ,产品ID desc NULLS LAST ';
      END IF;
      IF I_SORT = 5 THEN
        V_SORT := '产品期限 DESC ,产品ID desc NULLS LAST ';
      END IF;
      IF I_SORT = 6 THEN
        V_SORT := '产品期限  ASC ,产品ID desc NULLS LAST ';
      END IF;
    ELSE
      V_SORT := ' 是否首页展示 DESC ,产品ID desc NULLS LAST';
    END IF;

    V_WHERE := V_WHERE || ' ORDER BY ' || V_SORT;
    V_SQL   := V_SQL || V_WHERE;


 --1|首页——视频双录
  ELSIF I_QUERY_TYPE = 1 THEN

    --筛选客户
    V_WHERE:=' AND 1=1 ';
    IF I_CUST_NO LIKE '%1%' AND V_QZKHID<>-99 THEN
       V_WHERE:=V_WHERE||' AND (YY.KHH='||V_KHH||' OR YY.QZKHID='||V_QZKHID||')';
    ELSIF I_CUST_NO LIKE '%1%' AND V_QZKHID=-99 THEN
       V_WHERE:=V_WHERE||' AND YY.KHH='||V_KHH;
    ELSIF I_CUST_NO LIKE '%2%' THEN
       V_WHERE:=V_WHERE||' AND YY.QZKHID='||V_KHH;
    ELSE
        OPEN CUR_RESULT FOR
        SELECT -1 AS CODE, '客户号不允许为空' NOTE
        FROM DUAL;
        RETURN;
    END IF;

     -- -1|已关闭;1|排队中;2|待签约;3|待成立
    V_SQL_MJ := '
           SELECT
           YY.ID   预约ID,
           YY.GLSP 视频ID,
           CASE WHEN YY.CZYYTJ=4 THEN XM.CPMC_APP ELSE XM.XMMC END 产品名称,
           CASE WHEN NVL(FP.SFJZ,0)=0 THEN (SELECT SYJB.SYL FROM TFP_CPSYJB SYJB WHERE SYJB.ID=YY.SYJB)||''%''
                WHEN NVL(FP.SFJZ,0)=1 THEN ''浮动''

           END  "业绩比较基准",
           NVL(FP.SFJZ,0) 是否净值,
           YY.YYGMJE/10000 预约金额,  --万元
           NULL 持有份额,
           --(SELECT SYJB.QX||SYJB.QXDW FROM TFP_CPSYJB SYJB WHERE SYJB.ID=YY.SYJB) 产品期限,
               CASE WHEN FP.CPQZLX=0 THEN ''无期限''
                   WHEN FP.CPQZLX=1 THEN TO_CHAR(ROUND(FP.CPQX/30))
                  WHEN FP.CPQZLX=3 THEN TO_CHAR(ROUND(FP.CPQX/12))
                  ELSE TO_CHAR(ROUND(FP.CPQX))
             END  产品期限,
           (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) 产品类型,
           --预约状态ID
           CASE WHEN YY.YYZT=-1 THEN -1
                WHEN YY.YYZT=1 THEN 1
                WHEN YY.YYZT=2 AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)=0
                     THEN 2
                WHEN YY.YYZT=2 AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)>0
                     THEN 3
                END 预约状态ID,

           --预约状态
           CASE WHEN YY.YYZT=-1 THEN ''已关闭''
                WHEN YY.YYZT=1 THEN ''排队中''
                WHEN YY.YYZT=2 AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)=0
                     THEN ''待签约''
                WHEN YY.YYZT=2 AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)>0
                     THEN ''待成立''
                END 预约状态,


           (SELECT SP.FJH FROM TSPXXB SP WHERE SP.ID=YY.GLSP) 双录房间号,
           (SELECT SP.ANYCHATID FROM TSPXXB SP WHERE SP.ID=YY.GLSP) ANYCHATID

       FROM TFP_CPDM FP ,TCPYY YY,TXSXM XM
       WHERE FP.ID=YY.CPID AND YY.XSXM=XM.ID AND YY.YYZT!=-1
       AND ( YY.GLSP IS NULL OR (SELECT COUNT(1) FROM TSPXXB SP WHERE SP.CPYYID=YY.ID AND LZZT IN(1,2)AND SHZT!=-1)=0  )
       AND NOT EXISTS(SELECT 1 from THTXX HT WHERE HT.CPYYID=YY.ID) '||V_WHERE ;

    --4|已成立
    V_SQL_CX := ' SELECT
           HT.CPYYID 预约ID,
           YY.GLSP 视频ID,
           CASE WHEN YY.CZYYTJ=4 THEN XM.CPMC_APP ELSE NVL(XM.XMMC,FP.CPQC) END 产品名称,
           CASE WHEN NVL(FP.SFJZ,0)=0 THEN (SELECT SYJB.SYL FROM TFP_CPSYJB SYJB WHERE SYJB.ID=YY.SYJB)||''%''
                WHEN NVL(FP.SFJZ,0)=1 THEN ''浮动''

           END  "业绩比较基准",
           NVL(FP.SFJZ,0) 是否净值,
           NULL 预约金额,
           HT.HTFE/10000 持有份额,
           CASE WHEN HT.CPQZLX=0 THEN ''无期限''
                WHEN HT.CPQZLX=1 THEN TO_CHAR(ROUND(HT.CPQX/30))
                WHEN HT.CPQZLX=3 THEN TO_CHAR(HT.CPQX*12)
                ELSE TO_CHAR(HT.CPQX)
           END  产品期限,
           (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) 产品类型,
           4 预约状态ID,
           ''已成立'' 预约状态,
           (SELECT SP.FJH FROM TSPXXB SP WHERE YY.GLSP=SP.ID ) 双录房间号,
           (SELECT SP.ANYCHATID FROM TSPXXB SP WHERE YY.GLSP=SP.ID ) ANYCHATID

       FROM THTXX HT LEFT JOIN TCPYY YY ON HT.CPYYID= YY.ID LEFT JOIN TXSXM XM ON HT.XSXM=XM.ID LEFT JOIN THTSYR SYR ON SYR.HTXX=HT.ID,/*THTSYR SYR,*/TFP_CPDM FP
       where /*SYR.HTXX=HT.ID AND*/ HT.CPID=FP.ID   AND FP.CLRQ IS NOT NULL /* 20190319 YANGPEI 产品成立*/
       AND ( YY.GLSP IS NULL OR (SELECT COUNT(1) FROM TSPXXB SP WHERE SP.CPYYID=YY.ID AND LZZT IN(1,2)AND SHZT!=-1)=0  )
       AND HT.HTZT=3 AND '||I_CUST_NO||' LIKE ''1%'' AND HT.KHH='||V_KHH;

    --拼接 视频双录列表
    V_SQL   := 'SELECT * FROM ( '||V_SQL_MJ||' UNION ALL '||V_SQL_CX||') ORDER BY NVL(双录房间号,0) DESC,REPLACE(预约状态ID,-1,99) ASC,预约ID DESC ';

    V_COL_LST := '预约ID,视频ID,产品名称,业绩比较基准,是否净值,预约金额,持有份额,产品期限,产品类型,预约状态ID,预约状态,双录房间号,ANYCHATID';


    --2|财富——募集
  ELSIF I_QUERY_TYPE = 2 THEN

    --筛选客户
    V_WHERE:=' AND 1=1 ';
    IF I_CUST_NO LIKE '%1%' AND V_QZKHID<>-99 THEN
       V_WHERE:=V_WHERE||' AND (YY.KHH='||V_KHH||' OR YY.QZKHID='||V_QZKHID||')';
    ELSIF I_CUST_NO LIKE '%1%' AND V_QZKHID=-99 THEN
       V_WHERE:=V_WHERE||' AND YY.KHH='||V_KHH;
    ELSIF I_CUST_NO LIKE '%2%' THEN
       V_WHERE:=V_WHERE||' AND YY.QZKHID='||V_KHH;
    ELSE
        OPEN CUR_RESULT FOR
        SELECT -1 AS CODE, '客户号不允许为空' NOTE
        FROM DUAL;
        RETURN;
    END IF;

    --- -1|已关闭;1|排队中;2|待签约;3|待成立
    --获取系统路径
  SELECT T.PARAMVALUE||'TCUST_APP_KHQYLS/'''
    INTO V_PATH
    FROM TSYSPARAM T
   WHERE T.PARAMNAME = 'document-path';

    V_SQL := ' SELECT * FROM
          (SELECT FP.ID 产品ID,
           FP.CPDM 产品代码,
           CASE WHEN YY.CZYYTJ=4 THEN XM.CPMC_APP ELSE XM.XMMC END 产品名称,
           CASE WHEN YY.CZYYTJ=4 THEN XM.CPMC_APP ELSE XM.XMMC END 产品全称,
           YY.ID   预约ID,
           YY.GLSP 视频ID,
           ''DZHT_''||YY.ID||''.pdf''  电子合同名称,
           '''||V_PATH ||'||'||' (SELECT HT.ID FROM TCUST_APP_KHQYLS HT WHERE HT.CPYYID=YY.ID)' || '||''.FJ'' 电子合同存放路径,
           CASE WHEN NVL(FP.SFJZ,0)=0 THEN (SELECT decode(SYJB.SYL,0,''浮动'',SYJB.SYL||''%'') FROM TFP_CPSYJB SYJB WHERE SYJB.ID=YY.SYJB)
             --金融产品受益率为0时,显示浮动
            WHEN NVL(FP.SFJZ,0)=1 THEN ''浮动''

           END  "业绩比较基准",
           NVL(FP.SFJZ,0) 是否净值,
           YY.YYGMJE/10000 预约金额,  --万元
           (SELECT SYJB.QX||SYJB.QXDW FROM TFP_CPSYJB SYJB WHERE SYJB.ID=YY.SYJB) 产品期限,

           --预约状态ID
           CASE WHEN YY.YYZT=-1 THEN -1
                WHEN YY.YYZT=1 THEN 1
                WHEN YY.YYZT=2 AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)=0
                  AND (SELECT COUNT(1) FROM lchtrsgsqbd BD WHERE BD.CPYYID=YY.ID)=0
                     THEN 2
                WHEN YY.YYZT=2 AND ( (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)>0
                  OR (SELECT COUNT(1) FROM lchtrsgsqbd BD WHERE BD.CPYYID=YY.ID and shzt<>-1)>0)
                     THEN 3
                END 预约状态ID,

           --预约状态
           CASE WHEN YY.YYZT=-1 THEN ''已关闭''
                WHEN YY.YYZT=1 THEN ''排队中''
                WHEN YY.YYZT=2 AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)=0
                  AND (SELECT COUNT(1) FROM lchtrsgsqbd BD WHERE BD.CPYYID=YY.ID)=0
                     THEN ''待签约''
                WHEN YY.YYZT=2 AND ( (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)>0
                  OR (SELECT COUNT(1) FROM lchtrsgsqbd BD WHERE BD.CPYYID=YY.ID and shzt<>-1)>0)
                     THEN ''待成立''
                END 预约状态,

           --是否可修改金额 0|否;1|是
           CASE WHEN ( YY.YYZT=1 /*AND XM.SJBZ=1*/ AND (SELECT COUNT(1) FROM TCUST_APP_YYXG XG WHERE XG.CPYYID=YY.ID AND SHZT IN(0,1))=0 )
                     OR
                       ( YY.YYZT=2
                                   AND (SELECT COUNT(1) FROM TCUST_APP_YYXG XG WHERE XG.CPYYID=YY.ID AND SHZT IN(0,1))=0
                                   AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS LS WHERE LS.CPYYID=YY.ID)=0
                                   AND (SELECT COUNT(1) FROM TSPXXB SP WHERE SP.ID=YY.GLSP AND LZZT IN(1,2))=0
                                   /*AND XM.SJBZ=1*/
                        )
                     THEN 1 ELSE 0
                END  是否可修改金额,

           --是否允许双录 0|否;1|是
           CASE WHEN  YY.YYZT!=-1 AND (SELECT COUNT(1) FROM TCUST_APP_YYXG XG WHERE XG.CPYYID=YY.ID AND SHZT IN(0,1))=0
                     AND (SELECT COUNT(1) FROM TSPXXB SP WHERE SP.CPYYID=YY.ID AND LZZT IN(1,2))=0

                     THEN 1 ELSE 0
                END  是否允许双录,

           --是否允许签约 0|否;1|是
           CASE WHEN YY.YYZT=2 AND YY.CZYYTJ=4
                     --WENGJIE 20190121 屏蔽上架控制、新增必须有模板控制
                     --AND XM.SJBZ=1
                     AND EXISTS(select 1 from TDZHTYSMB MB WHERE MB.CPID in (SELECT ID  FROM  TFP_CPDM F WHERE XTJH=FP.XTJH) AND SFSX=1)
                     --YANGPEI 20190319 获取之前的合同模板
                     AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)=0
                     AND (SELECT COUNT(1) FROM LCHTRSGSQBD BD WHERE BD.CPYYID=YY.ID AND SHZT<>-1)=0
                     AND (SELECT COUNT(1) FROM TCUST_APP_YYXG XG WHERE XG.CPYYID=YY.ID AND SHZT IN(0,1))=0
                     AND (YY.RYXX IS NOT NULL AND NOT EXISTS(SELECT 1 FROM T_CUST_APP_GXFP WHERE USR_ID=YY.USR_ID AND FPZT!=3 ) )
                     THEN 1 ELSE 0
                END  是否允许签约,

           (SELECT NVL(MAX(LZZT),0) FROM TSPXXB SP WHERE SP.ID=YY.GLSP) 录制状态,   --0|未录制;1|已录制;2|已上传;
           (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) 产品类型,
           (SELECT SP.FJH FROM TSPXXB SP WHERE SP.ID=YY.GLSP) 双录房间号,
           (SELECT SP.ANYCHATID FROM TSPXXB SP WHERE SP.ID=YY.GLSP) ANYCHATID,
           CASE WHEN (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)=0 THEN 0 ELSE 1 END 是否查看合同
       FROM TFP_CPDM FP ,TCPYY YY,TXSXM XM
       WHERE FP.ID=YY.CPID AND YY.XSXM=XM.ID AND YY.YYZT<>-1
       AND (FP.CPZT=1 /* 20190319 YANGPEI 产品未成立即显示在募集中*/
       OR  NOT EXISTS(SELECT 1 from THTXX HT WHERE HT.CPYYID=YY.ID )  )  '
       ||V_WHERE ||')';


    V_SQL   := V_SQL || ' ORDER BY NVL(双录房间号,0) DESC,REPLACE(预约状态ID,-1,99) ASC,预约ID DESC ';

    V_COL_LST := '产品ID,产品代码, 产品名称, 产品全称,预约ID,是否查看合同, 视频ID,电子合同名称,电子合同存放路径, 业绩比较基准,是否净值, 预约金额, 产品期限, 预约状态ID, 预约状态,是否可修改金额,是否允许双录,是否允许签约, 录制状态,产品类型,双录房间号,ANYCHATID';

    --3|财富——存续
  ELSIF I_QUERY_TYPE = 3 THEN
    V_SQL := ' SELECT
           HT.ID 受益人ID ,
           YY.ID 预约ID,
           YY.GLSP 视频ID,
           HT.ID 合同ID,
           HT.HTXH 合同编号,
           FP.ID 产品ID,
           CASE WHEN YY.CZYYTJ=4 THEN XM.CPMC_APP ELSE NVL(XM.XMMC,FP.CPQC) END 产品名称,
           --HT.YQSYL||''%''  "业绩比较基准",
            decode(HT.YQSYL,0,''浮动'',HT.YQSYL||''%'') "业绩比较基准", /*受益率为0时显示浮动 */
           NVL(FP.SFJZ,0) 是否净值,
           HT.HTFE/10000 持有份额,
          (SELECT SYJB.QX||SYJB.QXDW FROM TFP_CPSYJB SYJB WHERE SYJB.ID=HT.SYJB) 产品期限,

           (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) 产品类型,

           --是否允许双录 0|否;1|是
           CASE WHEN YY.YYZT!=-1 AND (SELECT COUNT(1) FROM TSPXXB SP WHERE SP.CPYYID=YY.ID AND LZZT IN(1,2))=0
                     THEN 1 ELSE 0
                END  是否允许双录,

           (SELECT SP.FJH FROM TSPXXB SP WHERE YY.GLSP=SP.ID ) 双录房间号,
           (SELECT SP.ANYCHATID FROM TSPXXB SP WHERE YY.GLSP=SP.ID ) ANYCHATID

       FROM THTXX HT LEFT JOIN TCPYY YY ON HT.CPYYID= YY.ID LEFT JOIN TXSXM XM ON HT.XSXM=XM.ID LEFT JOIN THTSYR SYR ON SYR.HTXX=HT.ID,TFP_CPDM FP
       WHERE   HT.CPID=FP.ID  AND HT.HTZT=3 AND FP.CPZT IN (2,3) /* 20190319 YANGPEI 产品成立即显示在存续中*/
       AND FP.ID NOT IN(29135,28904) AND '||I_CUST_NO||' LIKE ''1%'' AND HT.KHH='||V_KHH;

    V_COL_LST := '受益人ID ,合同ID,产品ID, 产品名称,合同编号, 业绩比较基准, 持有份额,产品期限,是否净值,产品类型,预约ID,视频ID,是否允许双录,双录房间号,ANYCHATID ';


    --4|财富——清算
  ELSIF I_QUERY_TYPE = 4 THEN
   V_SQL := ' SELECT HT.ID 合同ID,
           HT.HTXH 合同编号,
           HT.ID 受益人ID ,
           FP.ID 产品ID,
           CASE WHEN XM.SJBZ IS NOT NULL THEN XM.CPMC_APP ELSE NVL(XM.XMMC,FP.CPQC) END 产品名称,
           --HT.YQSYL||''%''  "业绩比较基准",
           decode(HT.YQSYL,0,''浮动'',HT.YQSYL||''%'') "业绩比较基准", /*受益率为0时显示浮动 */
           NVL(FP.SFJZ,0) 是否净值,
           HT.HTFE/10000 历史持有份额,
           FP.DQRQ 产品结束日期,
           (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) 产品类型
       FROM THTXX HT LEFT JOIN  TXSXM XM ON HT.XSXM=XM.ID LEFT JOIN THTSYR SYR ON SYR.HTXX=HT.ID,TFP_CPDM FP
       where /*SYR.HTXX=HT.ID AND*/ HT.CPID=FP.ID
       AND  HT.HTZT IN(4,5,6,7) AND FP.ID NOT IN(29135,28904) AND '||I_CUST_NO||' LIKE ''1%'' AND HT.KHH='||V_KHH;

    V_COL_LST := '合同ID,产品ID,受益人ID , 产品名称,合同编号, 业绩比较基准, 历史持有份额,是否净值,产品结束日期,产品类型';
  END IF;


  --DBMS_OUTPUT.PUT_LINE(V_SQL);
  delete from cx_sql where bbid=909;
  insert into cx_sql(bbid,txt)  values(909,V_SQL);
  commit;
  --返回结果集
  PCX_JK_CX(CUR_RESULT,
            I_PAGENUMBER,
            I_PAGECOUNT,
            O_COUNT,
            V_COL_LST,
            V_SQL);
EXCEPTION
  WHEN OTHERS THEN
    BEGIN
      V_ERR_MSG := SQLERRM;
      OPEN CUR_RESULT FOR
        SELECT -1 AS CODE, '查询出错！错误：' || V_ERR_MSG AS NOTE
          FROM DUAL;
    END;
END PCX_CUST_APP_FP_LST;
/
