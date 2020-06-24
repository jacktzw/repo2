CREATE OR REPLACE PROCEDURE PCX_CUST_APP_FP_LST(CUR_RESULT   OUT TYPES.CURSORTYPE, --����α�
                                                I_PAGENUMBER IN NUMBER,   --ҳ��
                                                I_PAGECOUNT  IN NUMBER,   --ÿҳ����
                                                O_COUNT      OUT NUMBER,  --������ܼ�¼��
                                                I_SORT       IN VARCHAR2, --�����ֶ�
                                                I_QUERY_TYPE IN NUMBER,   --��ѯ����(0|ȫ��;1|��Ƶ˫¼;2|ļ��;3|����;4|����)
                                                I_CUST_NO    IN VARCHAR2, --�ͻ���
                                                I_USR_ID     IN NUMBER,   --APP_ID
                                                I_FP_NM      IN VARCHAR2, --��Ʒ����
                                                I_FP_BQ      IN NUMBER,   --��Ʒ��ǩ (1|�������;2|��Ʒ�Ƽ�;3|��Ʒ����)
                                                I_FP_TYPE    IN NUMBER,   --��Ʒ���� (1|����;0|�̶�)
                                                I_CPQX       IN NUMBER,   --��Ʒ���� (1:0-3���£�2:3-6���� 3:6-12���£� 4:12-24���� 5��24��������)
                                                I_CPSYL      IN NUMBER,   --��Ʒ������ (1:3%-5%��2:5%-7% 3:7% -9%�� 4:9%����)
                                                I_TZLY       IN NUMBER     --Ͷ����ҵ
 ) IS
  /******************************************************************************
    ��Ŀ���ƣ����пͻ�APP
    ������Ա��WENGJIE
    �������ڣ�2018-03-26
    ����˵����
      ����Ʒ���Ͳ�ѯ��Ʒ�б�
    ---------------------------------------------------------------------------

    �޸���        �汾��        �޸�����        ˵��
    WENGJIE       1.0.0         2018-03-26      ����
    WENGJIE       99.99         2018-07-08      ���������������зݶ�
    WENGJIE       99.99         2018-08-15      ��ͨ����
  ******************************************************************************/
  V_CUST_ID NUMBER(16);

  V_SQL     VARCHAR2(32767);
  V_SQL_MJ  VARCHAR2(32767);   --ļ��δ¼���б�
  V_SQL_CX  VARCHAR2(32767);   --����δ¼���б�
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

    --0|��ҳ������Ʒ
  IF I_QUERY_TYPE = 0 THEN
    IF I_USR_ID IS NOT NULL THEN
      V_SQL := '
           SELECT * FROM
           (
             SELECT FP.ID   ��ƷID,
             FP.CPDM ��Ʒ����,
             XM.CPMC_APP ��Ʒ����,
             XM.CPMC_APP ��Ʒȫ��,
             CASE WHEN (NVL(FP.SFJZ,0)=0 AND (SELECT COUNT(1) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )>1) THEN
                    (SELECT TRUNC(NVL(MIN(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )||''%-''||
                    (SELECT TRUNC(NVL(MAX(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )||''%''
                   WHEN (NVL(FP.SFJZ,0)=0 AND (SELECT COUNT(1) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )<=1) THEN
                     --(SELECT TRUNC(NVL(MAX(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )||''%''
                   (SELECT decode( TRUNC(NVL(MAX(JB.SYL), 0), 2),0,''����'',TRUNC(NVL(MAX(JB.SYL), 0), 2)|| ''%'') FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )

                WHEN NVL(FP.SFJZ,0)=1 THEN ''����''

               END  "ҵ���Ƚϻ�׼",
             (SELECT TRUNC(NVL(MIN(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID) ���������,
             (SELECT TRUNC(NVL(MAX(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID) ���������,
             CASE WHEN FP.CPQZLX=0 THEN 0
                  WHEN FP.CPQZLX=1 THEN ROUND(FP.CPQX/30)
                  WHEN FP.CPQZLX=3 THEN ROUND(FP.CPQX/12)
                  ELSE ROUND(FP.CPQX)
             END  ��Ʒ����,
             XM.GRRGQD/10000  ��Ͷ���,
             NVL(FP.SFJZ,0) �Ƿ�ֵ,
             (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) ��Ʒ����,
             XM.CPBQ ��Ʒ��ǩ,
             XM.SFSYZS �Ƿ���ҳչʾ
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
          /* 20190409 YANGPEI �ϼ�ʱ��ǰ��app����ʾ*/
            )';
    --�οͲ�ѯ��ҳ��Ʒ
    ELSE
      V_SQL := 'select * from
           (SELECT FP.ID   ��ƷID,
           FP.CPDM ��Ʒ����,
           XM.CPMC_APP ��Ʒ����,
           XM.CPMC_APP ��Ʒȫ��,
           CASE WHEN (NVL(FP.SFJZ,0)=0 AND (SELECT COUNT(1) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )>1) THEN
                    (SELECT TRUNC(NVL(MIN(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )||''%-''||
                    (SELECT TRUNC(NVL(MAX(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )||''%''
               -- WHEN (NVL(FP.SFJZ,0)=0 AND (SELECT COUNT(1) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)=0)>0)  THEN ''����'' /*����������Ϊ0��ʾ����  */
                WHEN (NVL(FP.SFJZ,0)=0 AND (SELECT COUNT(1) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )<=1) THEN
                     (SELECT  decode( TRUNC(NVL(MAX(JB.SYL), 0), 2),0,''����'',TRUNC(NVL(MAX(JB.SYL), 0), 2)|| ''%'') FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID AND NVL(JB.SYL,0)>0 )
                WHEN NVL(FP.SFJZ,0)=1 THEN ''����''

               END  "ҵ���Ƚϻ�׼",
           (SELECT TRUNC(NVL(MIN(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID) ���������,
           (SELECT TRUNC(NVL(MAX(JB.SYL),0),2) FROM TFP_CPSYJB JB WHERE JB.CPID=FP.ID) ���������,
           CASE WHEN FP.CPQZLX=0 THEN 0
                  WHEN FP.CPQZLX=1 THEN ROUND(FP.CPQX/30)
                  WHEN FP.CPQZLX=3 THEN ROUND(FP.CPQX/12)
                  ELSE ROUND(FP.CPQX)
             END  ��Ʒ����,
           XM.GRRGQD/10000  ��Ͷ���,
           NVL(FP.SFJZ,0) �Ƿ�ֵ,
           (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) ��Ʒ����,
           XM.CPBQ ��Ʒ��ǩ,
           XM.SFSYZS �Ƿ���ҳչʾ
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
       /* 20190409 YANGPEI �ϼ�ʱ��ǰ��app����ʾ*/
       )';
    END IF;

    V_COL_LST := '��ƷID, ��Ʒ����, ��Ʒ����, ��Ʒȫ��, ҵ���Ƚϻ�׼ , ��Ͷ��� , ��Ʒ����, �Ƿ�ֵ, ��Ʒ����,���������,��Ʒ����   ';
    --ȫ����Ʒɸѡ����

    V_WHERE :=' WHERE 1=1 ';
    --��Ʒɸѡ
    --1|���� 0|�̶�
    IF I_FP_TYPE IS NOT NULL THEN
      V_WHERE := V_WHERE ||
                 ' AND �Ƿ�ֵ = ' ||
                 I_FP_TYPE;
    END IF;

    --��Ʒ��ǩ
    IF I_FP_BQ IS NOT NULL THEN
       V_WHERE := V_WHERE ||
                 ' AND ��Ʒ��ǩ = ' ||
                 I_FP_BQ;
    END IF;

    --Ͷ����ҵ
    IF I_TZLY IS NOT NULL THEN
       SELECT ''''||NOTE||'''' INTO V_TZLY FROM TXTDM ZD WHERE FLDM='TZLY' AND ZD.IBM=I_TZLY;
       V_WHERE := V_WHERE ||
                 ' AND ��Ʒ���� = ' ||
                 V_TZLY;
    END IF;

    --��Ʒ����ɸѡ
    IF I_FP_NM IS NOT NULL THEN
      V_WHERE := V_WHERE || ' AND ��Ʒȫ�� LIKE(''%' || I_FP_NM || '%'')';
    END IF;

    --��Ʒ����ɸѡ (1:0-3���£�2:3-6���� 3:6-12���£� 4:12-24���� 5��24��������)
    IF I_CPQX IS NOT NULL THEN
      IF I_CPQX = 1 THEN
        V_WHERE := V_WHERE || ' AND ��Ʒ���� > 0 AND ��Ʒ���� <= 3 ';
      END IF;
      IF I_CPQX = 2 THEN
        V_WHERE := V_WHERE || ' AND ��Ʒ���� > 3 AND ��Ʒ���� <= 6 ';
      END IF;
      IF I_CPQX = 3 THEN
        V_WHERE := V_WHERE || ' AND ��Ʒ���� > 6 AND ��Ʒ���� <= 12 ';
      END IF;
      IF I_CPQX = 4 THEN
        V_WHERE := V_WHERE || ' AND ��Ʒ���� > 12 AND ��Ʒ���� <= 24';
      END IF;
      IF I_CPQX = 5 THEN
        V_WHERE := V_WHERE || ' AND ��Ʒ���� > 24 ';
      END IF;
       V_WHERE := V_WHERE;
    END IF;

    --��Ʒ������ɸѡ ( 2:5%-7%  3:7% -9% 4:9%����)
    IF I_CPSYL IS NOT NULL THEN
      IF I_CPSYL = 2 THEN
        V_WHERE := V_WHERE || ' AND ��������� > 5 AND ��������� <=7 ';
      END IF;
      IF I_CPSYL = 3 THEN
        V_WHERE := V_WHERE || ' AND ��������� > 7 AND ��������� <=9 ';
      END IF;
      IF I_CPSYL = 4 THEN
        V_WHERE := V_WHERE || ' AND ��������� > 9 ';
      END IF;
     V_WHERE := V_WHERE;
    END IF;


    --���� 1,2|�ۺ�������; 3,4|�����ʵ���; 5,6|���޵���
    IF I_SORT IS NOT NULL THEN
      IF I_SORT = 1 THEN
        V_SORT := '��Ͷ��� DESC,ҵ���Ƚϻ�׼ DESC ,��ƷID desc NULLS LAST ';
      END IF;
      IF I_SORT = 2 THEN
        V_SORT := '��Ͷ��� ASC,ҵ���Ƚϻ�׼ ASC ,��ƷID desc NULLS LAST ';
      END IF;
      IF I_SORT = 3 THEN
        V_SORT := '��������� DESC ,��ƷID desc NULLS LAST ';
      END IF;
      IF I_SORT = 4 THEN
        V_SORT := '���������  ASC ,��ƷID desc NULLS LAST ';
      END IF;
      IF I_SORT = 5 THEN
        V_SORT := '��Ʒ���� DESC ,��ƷID desc NULLS LAST ';
      END IF;
      IF I_SORT = 6 THEN
        V_SORT := '��Ʒ����  ASC ,��ƷID desc NULLS LAST ';
      END IF;
    ELSE
      V_SORT := ' �Ƿ���ҳչʾ DESC ,��ƷID desc NULLS LAST';
    END IF;

    V_WHERE := V_WHERE || ' ORDER BY ' || V_SORT;
    V_SQL   := V_SQL || V_WHERE;


 --1|��ҳ������Ƶ˫¼
  ELSIF I_QUERY_TYPE = 1 THEN

    --ɸѡ�ͻ�
    V_WHERE:=' AND 1=1 ';
    IF I_CUST_NO LIKE '%1%' AND V_QZKHID<>-99 THEN
       V_WHERE:=V_WHERE||' AND (YY.KHH='||V_KHH||' OR YY.QZKHID='||V_QZKHID||')';
    ELSIF I_CUST_NO LIKE '%1%' AND V_QZKHID=-99 THEN
       V_WHERE:=V_WHERE||' AND YY.KHH='||V_KHH;
    ELSIF I_CUST_NO LIKE '%2%' THEN
       V_WHERE:=V_WHERE||' AND YY.QZKHID='||V_KHH;
    ELSE
        OPEN CUR_RESULT FOR
        SELECT -1 AS CODE, '�ͻ��Ų�����Ϊ��' NOTE
        FROM DUAL;
        RETURN;
    END IF;

     -- -1|�ѹر�;1|�Ŷ���;2|��ǩԼ;3|������
    V_SQL_MJ := '
           SELECT
           YY.ID   ԤԼID,
           YY.GLSP ��ƵID,
           CASE WHEN YY.CZYYTJ=4 THEN XM.CPMC_APP ELSE XM.XMMC END ��Ʒ����,
           CASE WHEN NVL(FP.SFJZ,0)=0 THEN (SELECT SYJB.SYL FROM TFP_CPSYJB SYJB WHERE SYJB.ID=YY.SYJB)||''%''
                WHEN NVL(FP.SFJZ,0)=1 THEN ''����''

           END  "ҵ���Ƚϻ�׼",
           NVL(FP.SFJZ,0) �Ƿ�ֵ,
           YY.YYGMJE/10000 ԤԼ���,  --��Ԫ
           NULL ���зݶ�,
           --(SELECT SYJB.QX||SYJB.QXDW FROM TFP_CPSYJB SYJB WHERE SYJB.ID=YY.SYJB) ��Ʒ����,
               CASE WHEN FP.CPQZLX=0 THEN ''������''
                   WHEN FP.CPQZLX=1 THEN TO_CHAR(ROUND(FP.CPQX/30))
                  WHEN FP.CPQZLX=3 THEN TO_CHAR(ROUND(FP.CPQX/12))
                  ELSE TO_CHAR(ROUND(FP.CPQX))
             END  ��Ʒ����,
           (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) ��Ʒ����,
           --ԤԼ״̬ID
           CASE WHEN YY.YYZT=-1 THEN -1
                WHEN YY.YYZT=1 THEN 1
                WHEN YY.YYZT=2 AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)=0
                     THEN 2
                WHEN YY.YYZT=2 AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)>0
                     THEN 3
                END ԤԼ״̬ID,

           --ԤԼ״̬
           CASE WHEN YY.YYZT=-1 THEN ''�ѹر�''
                WHEN YY.YYZT=1 THEN ''�Ŷ���''
                WHEN YY.YYZT=2 AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)=0
                     THEN ''��ǩԼ''
                WHEN YY.YYZT=2 AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)>0
                     THEN ''������''
                END ԤԼ״̬,


           (SELECT SP.FJH FROM TSPXXB SP WHERE SP.ID=YY.GLSP) ˫¼�����,
           (SELECT SP.ANYCHATID FROM TSPXXB SP WHERE SP.ID=YY.GLSP) ANYCHATID

       FROM TFP_CPDM FP ,TCPYY YY,TXSXM XM
       WHERE FP.ID=YY.CPID AND YY.XSXM=XM.ID AND YY.YYZT!=-1
       AND ( YY.GLSP IS NULL OR (SELECT COUNT(1) FROM TSPXXB SP WHERE SP.CPYYID=YY.ID AND LZZT IN(1,2)AND SHZT!=-1)=0  )
       AND NOT EXISTS(SELECT 1 from THTXX HT WHERE HT.CPYYID=YY.ID) '||V_WHERE ;

    --4|�ѳ���
    V_SQL_CX := ' SELECT
           HT.CPYYID ԤԼID,
           YY.GLSP ��ƵID,
           CASE WHEN YY.CZYYTJ=4 THEN XM.CPMC_APP ELSE NVL(XM.XMMC,FP.CPQC) END ��Ʒ����,
           CASE WHEN NVL(FP.SFJZ,0)=0 THEN (SELECT SYJB.SYL FROM TFP_CPSYJB SYJB WHERE SYJB.ID=YY.SYJB)||''%''
                WHEN NVL(FP.SFJZ,0)=1 THEN ''����''

           END  "ҵ���Ƚϻ�׼",
           NVL(FP.SFJZ,0) �Ƿ�ֵ,
           NULL ԤԼ���,
           HT.HTFE/10000 ���зݶ�,
           CASE WHEN HT.CPQZLX=0 THEN ''������''
                WHEN HT.CPQZLX=1 THEN TO_CHAR(ROUND(HT.CPQX/30))
                WHEN HT.CPQZLX=3 THEN TO_CHAR(HT.CPQX*12)
                ELSE TO_CHAR(HT.CPQX)
           END  ��Ʒ����,
           (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) ��Ʒ����,
           4 ԤԼ״̬ID,
           ''�ѳ���'' ԤԼ״̬,
           (SELECT SP.FJH FROM TSPXXB SP WHERE YY.GLSP=SP.ID ) ˫¼�����,
           (SELECT SP.ANYCHATID FROM TSPXXB SP WHERE YY.GLSP=SP.ID ) ANYCHATID

       FROM THTXX HT LEFT JOIN TCPYY YY ON HT.CPYYID= YY.ID LEFT JOIN TXSXM XM ON HT.XSXM=XM.ID LEFT JOIN THTSYR SYR ON SYR.HTXX=HT.ID,/*THTSYR SYR,*/TFP_CPDM FP
       where /*SYR.HTXX=HT.ID AND*/ HT.CPID=FP.ID   AND FP.CLRQ IS NOT NULL /* 20190319 YANGPEI ��Ʒ����*/
       AND ( YY.GLSP IS NULL OR (SELECT COUNT(1) FROM TSPXXB SP WHERE SP.CPYYID=YY.ID AND LZZT IN(1,2)AND SHZT!=-1)=0  )
       AND HT.HTZT=3 AND '||I_CUST_NO||' LIKE ''1%'' AND HT.KHH='||V_KHH;

    --ƴ�� ��Ƶ˫¼�б�
    V_SQL   := 'SELECT * FROM ( '||V_SQL_MJ||' UNION ALL '||V_SQL_CX||') ORDER BY NVL(˫¼�����,0) DESC,REPLACE(ԤԼ״̬ID,-1,99) ASC,ԤԼID DESC ';

    V_COL_LST := 'ԤԼID,��ƵID,��Ʒ����,ҵ���Ƚϻ�׼,�Ƿ�ֵ,ԤԼ���,���зݶ�,��Ʒ����,��Ʒ����,ԤԼ״̬ID,ԤԼ״̬,˫¼�����,ANYCHATID';


    --2|�Ƹ�����ļ��
  ELSIF I_QUERY_TYPE = 2 THEN

    --ɸѡ�ͻ�
    V_WHERE:=' AND 1=1 ';
    IF I_CUST_NO LIKE '%1%' AND V_QZKHID<>-99 THEN
       V_WHERE:=V_WHERE||' AND (YY.KHH='||V_KHH||' OR YY.QZKHID='||V_QZKHID||')';
    ELSIF I_CUST_NO LIKE '%1%' AND V_QZKHID=-99 THEN
       V_WHERE:=V_WHERE||' AND YY.KHH='||V_KHH;
    ELSIF I_CUST_NO LIKE '%2%' THEN
       V_WHERE:=V_WHERE||' AND YY.QZKHID='||V_KHH;
    ELSE
        OPEN CUR_RESULT FOR
        SELECT -1 AS CODE, '�ͻ��Ų�����Ϊ��' NOTE
        FROM DUAL;
        RETURN;
    END IF;

    --- -1|�ѹر�;1|�Ŷ���;2|��ǩԼ;3|������
    --��ȡϵͳ·��
  SELECT T.PARAMVALUE||'TCUST_APP_KHQYLS/'''
    INTO V_PATH
    FROM TSYSPARAM T
   WHERE T.PARAMNAME = 'document-path';

    V_SQL := ' SELECT * FROM
          (SELECT FP.ID ��ƷID,
           FP.CPDM ��Ʒ����,
           CASE WHEN YY.CZYYTJ=4 THEN XM.CPMC_APP ELSE XM.XMMC END ��Ʒ����,
           CASE WHEN YY.CZYYTJ=4 THEN XM.CPMC_APP ELSE XM.XMMC END ��Ʒȫ��,
           YY.ID   ԤԼID,
           YY.GLSP ��ƵID,
           ''DZHT_''||YY.ID||''.pdf''  ���Ӻ�ͬ����,
           '''||V_PATH ||'||'||' (SELECT HT.ID FROM TCUST_APP_KHQYLS HT WHERE HT.CPYYID=YY.ID)' || '||''.FJ'' ���Ӻ�ͬ���·��,
           CASE WHEN NVL(FP.SFJZ,0)=0 THEN (SELECT decode(SYJB.SYL,0,''����'',SYJB.SYL||''%'') FROM TFP_CPSYJB SYJB WHERE SYJB.ID=YY.SYJB)
             --���ڲ�Ʒ������Ϊ0ʱ,��ʾ����
            WHEN NVL(FP.SFJZ,0)=1 THEN ''����''

           END  "ҵ���Ƚϻ�׼",
           NVL(FP.SFJZ,0) �Ƿ�ֵ,
           YY.YYGMJE/10000 ԤԼ���,  --��Ԫ
           (SELECT SYJB.QX||SYJB.QXDW FROM TFP_CPSYJB SYJB WHERE SYJB.ID=YY.SYJB) ��Ʒ����,

           --ԤԼ״̬ID
           CASE WHEN YY.YYZT=-1 THEN -1
                WHEN YY.YYZT=1 THEN 1
                WHEN YY.YYZT=2 AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)=0
                  AND (SELECT COUNT(1) FROM lchtrsgsqbd BD WHERE BD.CPYYID=YY.ID)=0
                     THEN 2
                WHEN YY.YYZT=2 AND ( (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)>0
                  OR (SELECT COUNT(1) FROM lchtrsgsqbd BD WHERE BD.CPYYID=YY.ID and shzt<>-1)>0)
                     THEN 3
                END ԤԼ״̬ID,

           --ԤԼ״̬
           CASE WHEN YY.YYZT=-1 THEN ''�ѹر�''
                WHEN YY.YYZT=1 THEN ''�Ŷ���''
                WHEN YY.YYZT=2 AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)=0
                  AND (SELECT COUNT(1) FROM lchtrsgsqbd BD WHERE BD.CPYYID=YY.ID)=0
                     THEN ''��ǩԼ''
                WHEN YY.YYZT=2 AND ( (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)>0
                  OR (SELECT COUNT(1) FROM lchtrsgsqbd BD WHERE BD.CPYYID=YY.ID and shzt<>-1)>0)
                     THEN ''������''
                END ԤԼ״̬,

           --�Ƿ���޸Ľ�� 0|��;1|��
           CASE WHEN ( YY.YYZT=1 /*AND XM.SJBZ=1*/ AND (SELECT COUNT(1) FROM TCUST_APP_YYXG XG WHERE XG.CPYYID=YY.ID AND SHZT IN(0,1))=0 )
                     OR
                       ( YY.YYZT=2
                                   AND (SELECT COUNT(1) FROM TCUST_APP_YYXG XG WHERE XG.CPYYID=YY.ID AND SHZT IN(0,1))=0
                                   AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS LS WHERE LS.CPYYID=YY.ID)=0
                                   AND (SELECT COUNT(1) FROM TSPXXB SP WHERE SP.ID=YY.GLSP AND LZZT IN(1,2))=0
                                   /*AND XM.SJBZ=1*/
                        )
                     THEN 1 ELSE 0
                END  �Ƿ���޸Ľ��,

           --�Ƿ�����˫¼ 0|��;1|��
           CASE WHEN  YY.YYZT!=-1 AND (SELECT COUNT(1) FROM TCUST_APP_YYXG XG WHERE XG.CPYYID=YY.ID AND SHZT IN(0,1))=0
                     AND (SELECT COUNT(1) FROM TSPXXB SP WHERE SP.CPYYID=YY.ID AND LZZT IN(1,2))=0

                     THEN 1 ELSE 0
                END  �Ƿ�����˫¼,

           --�Ƿ�����ǩԼ 0|��;1|��
           CASE WHEN YY.YYZT=2 AND YY.CZYYTJ=4
                     --WENGJIE 20190121 �����ϼܿ��ơ�����������ģ�����
                     --AND XM.SJBZ=1
                     AND EXISTS(select 1 from TDZHTYSMB MB WHERE MB.CPID in (SELECT ID  FROM  TFP_CPDM F WHERE XTJH=FP.XTJH) AND SFSX=1)
                     --YANGPEI 20190319 ��ȡ֮ǰ�ĺ�ͬģ��
                     AND (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)=0
                     AND (SELECT COUNT(1) FROM LCHTRSGSQBD BD WHERE BD.CPYYID=YY.ID AND SHZT<>-1)=0
                     AND (SELECT COUNT(1) FROM TCUST_APP_YYXG XG WHERE XG.CPYYID=YY.ID AND SHZT IN(0,1))=0
                     AND (YY.RYXX IS NOT NULL AND NOT EXISTS(SELECT 1 FROM T_CUST_APP_GXFP WHERE USR_ID=YY.USR_ID AND FPZT!=3 ) )
                     THEN 1 ELSE 0
                END  �Ƿ�����ǩԼ,

           (SELECT NVL(MAX(LZZT),0) FROM TSPXXB SP WHERE SP.ID=YY.GLSP) ¼��״̬,   --0|δ¼��;1|��¼��;2|���ϴ�;
           (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) ��Ʒ����,
           (SELECT SP.FJH FROM TSPXXB SP WHERE SP.ID=YY.GLSP) ˫¼�����,
           (SELECT SP.ANYCHATID FROM TSPXXB SP WHERE SP.ID=YY.GLSP) ANYCHATID,
           CASE WHEN (SELECT COUNT(1) FROM TCUST_APP_KHQYLS BD WHERE BD.CPYYID=YY.ID)=0 THEN 0 ELSE 1 END �Ƿ�鿴��ͬ
       FROM TFP_CPDM FP ,TCPYY YY,TXSXM XM
       WHERE FP.ID=YY.CPID AND YY.XSXM=XM.ID AND YY.YYZT<>-1
       AND (FP.CPZT=1 /* 20190319 YANGPEI ��Ʒδ��������ʾ��ļ����*/
       OR  NOT EXISTS(SELECT 1 from THTXX HT WHERE HT.CPYYID=YY.ID )  )  '
       ||V_WHERE ||')';


    V_SQL   := V_SQL || ' ORDER BY NVL(˫¼�����,0) DESC,REPLACE(ԤԼ״̬ID,-1,99) ASC,ԤԼID DESC ';

    V_COL_LST := '��ƷID,��Ʒ����, ��Ʒ����, ��Ʒȫ��,ԤԼID,�Ƿ�鿴��ͬ, ��ƵID,���Ӻ�ͬ����,���Ӻ�ͬ���·��, ҵ���Ƚϻ�׼,�Ƿ�ֵ, ԤԼ���, ��Ʒ����, ԤԼ״̬ID, ԤԼ״̬,�Ƿ���޸Ľ��,�Ƿ�����˫¼,�Ƿ�����ǩԼ, ¼��״̬,��Ʒ����,˫¼�����,ANYCHATID';

    --3|�Ƹ���������
  ELSIF I_QUERY_TYPE = 3 THEN
    V_SQL := ' SELECT
           HT.ID ������ID ,
           YY.ID ԤԼID,
           YY.GLSP ��ƵID,
           HT.ID ��ͬID,
           HT.HTXH ��ͬ���,
           FP.ID ��ƷID,
           CASE WHEN YY.CZYYTJ=4 THEN XM.CPMC_APP ELSE NVL(XM.XMMC,FP.CPQC) END ��Ʒ����,
           --HT.YQSYL||''%''  "ҵ���Ƚϻ�׼",
            decode(HT.YQSYL,0,''����'',HT.YQSYL||''%'') "ҵ���Ƚϻ�׼", /*������Ϊ0ʱ��ʾ���� */
           NVL(FP.SFJZ,0) �Ƿ�ֵ,
           HT.HTFE/10000 ���зݶ�,
          (SELECT SYJB.QX||SYJB.QXDW FROM TFP_CPSYJB SYJB WHERE SYJB.ID=HT.SYJB) ��Ʒ����,

           (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) ��Ʒ����,

           --�Ƿ�����˫¼ 0|��;1|��
           CASE WHEN YY.YYZT!=-1 AND (SELECT COUNT(1) FROM TSPXXB SP WHERE SP.CPYYID=YY.ID AND LZZT IN(1,2))=0
                     THEN 1 ELSE 0
                END  �Ƿ�����˫¼,

           (SELECT SP.FJH FROM TSPXXB SP WHERE YY.GLSP=SP.ID ) ˫¼�����,
           (SELECT SP.ANYCHATID FROM TSPXXB SP WHERE YY.GLSP=SP.ID ) ANYCHATID

       FROM THTXX HT LEFT JOIN TCPYY YY ON HT.CPYYID= YY.ID LEFT JOIN TXSXM XM ON HT.XSXM=XM.ID LEFT JOIN THTSYR SYR ON SYR.HTXX=HT.ID,TFP_CPDM FP
       WHERE   HT.CPID=FP.ID  AND HT.HTZT=3 AND FP.CPZT IN (2,3) /* 20190319 YANGPEI ��Ʒ��������ʾ�ڴ�����*/
       AND FP.ID NOT IN(29135,28904) AND '||I_CUST_NO||' LIKE ''1%'' AND HT.KHH='||V_KHH;

    V_COL_LST := '������ID ,��ͬID,��ƷID, ��Ʒ����,��ͬ���, ҵ���Ƚϻ�׼, ���зݶ�,��Ʒ����,�Ƿ�ֵ,��Ʒ����,ԤԼID,��ƵID,�Ƿ�����˫¼,˫¼�����,ANYCHATID ';


    --4|�Ƹ���������
  ELSIF I_QUERY_TYPE = 4 THEN
   V_SQL := ' SELECT HT.ID ��ͬID,
           HT.HTXH ��ͬ���,
           HT.ID ������ID ,
           FP.ID ��ƷID,
           CASE WHEN XM.SJBZ IS NOT NULL THEN XM.CPMC_APP ELSE NVL(XM.XMMC,FP.CPQC) END ��Ʒ����,
           --HT.YQSYL||''%''  "ҵ���Ƚϻ�׼",
           decode(HT.YQSYL,0,''����'',HT.YQSYL||''%'') "ҵ���Ƚϻ�׼", /*������Ϊ0ʱ��ʾ���� */
           NVL(FP.SFJZ,0) �Ƿ�ֵ,
           HT.HTFE/10000 ��ʷ���зݶ�,
           FP.DQRQ ��Ʒ��������,
           (SELECT NOTE FROM TXTDM ZD WHERE FLDM=''TZLY'' AND ZD.IBM=FP.TZLY) ��Ʒ����
       FROM THTXX HT LEFT JOIN  TXSXM XM ON HT.XSXM=XM.ID LEFT JOIN THTSYR SYR ON SYR.HTXX=HT.ID,TFP_CPDM FP
       where /*SYR.HTXX=HT.ID AND*/ HT.CPID=FP.ID
       AND  HT.HTZT IN(4,5,6,7) AND FP.ID NOT IN(29135,28904) AND '||I_CUST_NO||' LIKE ''1%'' AND HT.KHH='||V_KHH;

    V_COL_LST := '��ͬID,��ƷID,������ID , ��Ʒ����,��ͬ���, ҵ���Ƚϻ�׼, ��ʷ���зݶ�,�Ƿ�ֵ,��Ʒ��������,��Ʒ����';
  END IF;


  --DBMS_OUTPUT.PUT_LINE(V_SQL);
  delete from cx_sql where bbid=909;
  insert into cx_sql(bbid,txt)  values(909,V_SQL);
  commit;
  --���ؽ����
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
        SELECT -1 AS CODE, '��ѯ��������' || V_ERR_MSG AS NOTE
          FROM DUAL;
    END;
END PCX_CUST_APP_FP_LST;
/
