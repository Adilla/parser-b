MACHINE
  id_tva(
    id_jii, id_lmu, id_txd, id_wpb, id_fwd, id_glq, id_jjn, id_uoi, id_qtt
  )

CONSTRAINTS
  #(id_xkl, id_hpn, id_hwv, id_kai).
  (not
     (!
        (id_uby, id_acl, id_lmu, id_ejs, id_rwl, id_wny, id_eel, id_mms,
         id_swo, id_ydo).
      (str_vnx /: str_nuy'id_koq or
       (str_kfi$0 <: str_rbn$0 => MININT /<: str_chy$0))))

SEES
  id_mbt

INCLUDES
  id_qzu,
  id_bny (
    ([]
       (bool
          ([INT, NATURAL1, (NATURAL1 <-> MAXINT)] <
           { id_axr, id_owd, id_rmv, id_rtb, id_tfq, id_xnl
             |
             str_ayu /<<: str_orp$0 }
             'id_zgx))),
    {INTER(id_dzi, id_qsa, id_mea, id_tvl, id_gym, id_bno, id_ykl, id_uay).
     (!(id_frt, id_fmu, id_sdw, id_ayj, id_nds, id_esy).
      (#
         (id_gzo, id_jdr, id_mzl, id_ikf, id_yof, id_wjv, id_svq, id_sko,
          id_vun, id_hyr, id_ehh).
       (str_gqy$0 /= str_bfo$0))|
      bool(not(!(id_tfm, id_gaw, id_glm).(str_knk /= STRING))))},
    (str_wzo
       ({ id_kfp, id_yzy, id_xqb, id_gjr, id_lur, id_nla, id_jpg, id_ofa,
          id_itv
          |
          #
            (id_ceq, id_inw, id_rur, id_dzh, id_spt, id_kit, id_hff, id_sii,
             id_ehg).
          (#
             (id_jmb, id_vwo, id_qdc, id_zog, id_tqg, id_qhb, id_nrj, id_jah,
              id_fms, id_adt).
           (str_ihg >= str_htc$0 &
            #
              (id_oer, id_ufh, id_lpy, id_uqk, id_xhn, id_dmp, id_zxj,
               id_irt, id_jme).
            (STRING <: str_jeu <=> str_lnj$0 /: MAXINT))) }))
  ),
  id_eap (
    UNION(id_zvt, id_dfi, id_wae).
    (str_duz$0 /<: str_tfh$0|
     {str_gcz$0, str_gzh$0, BOOL, NATURAL} , (4(str_exq$0 |> TRUE'id_nnn)))
      'id_iuy,
    INTER
      (id_bul, id_qtr, id_nsn, id_yru, id_inr, id_pqa, id_bqd, id_mzv,
       id_thl, id_ypp, id_kmc).
    (!(id_ifa, id_jdl, id_kvc, id_pkq, id_szu, id_aqq).
     (!(id_qcp, id_ojq, id_eev, id_dkv, id_ulm, id_huw, id_txm).
      (#(id_agg, id_ajk, id_jan, id_lng).
       (!
          (id_xdp, id_bqz, id_qks, id_sst, id_pps, id_qsk, id_fmu, id_deh,
           id_oid).
        (not(#(id_hln, id_yrq).(str_zly >= str_yth$0))))))|
     str_qlb),
    struct
      (id_uqd:
       (struct
          (id_jcb: str_hgn,
           id_map: str_mzm,
           id_gyk: INT,
           id_tuh: BOOL,
           id_qcd: STRING,
           id_pao: str_tpv,
           id_eys: str_tse$0) /\
        str_cep$0),
       id_spg: NAT,
       id_unc:
       (str_wyx
          [{ id_axr, id_eix, id_hby, id_bkt, id_iwd, id_pzu, id_oji
             |
             str_jiv$0 >= str_puq & str_ncl$0 /: str_hfi }]),
       id_put: str_uju$0,
       id_vgj: NAT,
       id_mfy: (STRING .. str_xfq$0),
       id_dre: str_vgg)
  ),
  id_iku (
    ((sizet(str_rvb$0(str_erp)))
       ({UNION(id_lxv). (MAXINT /= str_hpo$0| str_qbv),
         (subtree
            (INTER(id_cdf, id_suh, id_xrj, id_ypq, id_tzj, id_hoc).
             (str_qdw = str_jni$0| str_djn$0))),
         str_iiv$0,
         str_wpu}))
  ),
  id_hgw (
    {(pred
        (max
           ((%
               (id_myq, id_ztg, id_tso, id_hdp, id_szw, id_zya, id_gcg, id_teu).
             (str_ylu <: str_qxn| str_kya$0([]))
               (str_saw$0)) ->
            BOOL)))},
    { id_lfk
      |
      #(id_vdw, id_tml, id_arx, id_udv, id_gsr, id_mas, id_adq, id_hbx).
      (not
         ({str_vql, str_egs, str_fpk, str_hye, str_zsf$0, str_rco$0, BOOL,
           str_weh$0} <
          NATURAL1) <=>
       {} >= TRUE) }
      'id_omk
  ),
  id_she (
    ((MININT(str_gum))
       (rec
          (id_dgr: (sons(str_omt)),
           id_bro: INTEGER,
           id_ael: (mirror(MAXINT)),
           id_hbx:
           UNION
             (id_wsy, id_jvo, id_amo, id_gwt, id_rej, id_dbj, id_caq, id_khm).
           (#(id_euz, id_uvh, id_gay, id_ncl).(str_vvh$0 <= str_oxa)| INT),
           id_fud: str_njr))),
    [[],
     [struct
        (id_htc: NATURAL1,
         id_wvf: str_gcu$0,
         id_cbc: str_vaw,
         id_cfu: str_tic$0,
         id_sug: str_twy$0),
      bool(str_jbc <: str_nts$0),
      BOOL,
      (str_ioo , str_qcf$0)],
     str_ehf$0,
     rec
       (id_wik: NAT1,
        id_zvo: str_lxm,
        id_rmb: str_asc,
        id_kie: STRING,
        id_bcj: str_ivy),
     str_zgc'id_cbu,
     str_bvs$0,
     str_aro$0,
     str_lob$0]
  ),
  id_egw (
    ([str_rhg,
      {BOOL,
       PI(id_hhn, id_jqq, id_nth, id_egw, id_ony, id_shg).
       (str_heu$0 /<<: str_bhw| str_wll),
       str_krb,
       str_mxi,
       str_gxj,
       str_sso,
       struct
         (id_gvj: NAT1,
          id_nev: str_iyn$0,
          id_ngk: NAT,
          id_vfy: INTEGER,
          id_kxj: str_ftw$0,
          id_wsx: str_bfw,
          id_gtd: str_lvg)},
      str_vit$0,
      str_uvf$0,
      str_jzl$0,
      str_zgu,
      (str_qsk(NATURAL1)),
      str_gsv] |>>
     (str_sve ** str_vip)),
    [str_shi,
     bool
       (!(id_ikn, id_pre, id_cus, id_yyi).(NAT > str_duk) =>
        !(id_rvu, id_rbz).(NAT1 <: str_mli)),
     {str_zoa$0, bool(str_hdu$0 /<<: NATURAL), FALSE},
     str_djy]
  ),
  id_vkp (
    (rec
       (id_ruk:
        rec
          (id_ush: str_sfk$0,
           id_exg: "str_wce",
           id_zix: str_crr,
           id_uwq: str_qca$0,
           id_uca: str_tph,
           id_jpx: TRUE),
        id_pto: MAXINT,
        id_lml: str_ogl,
        id_lts:
        rec
          (id_xqk: str_smf,
           id_kse: str_mvo,
           id_kdx: str_snr,
           id_vgw: { id_odh | str_uzp /<<: str_qmp$0 },
           id_tol: str_hyl,
           id_shv: str_zbd,
           id_xde: str_hzy$0,
           id_hex: str_oty),
        id_ugk: {},
        id_ldk: (str_uws(BOOL))) |>
     bool(str_zxs <: str_oyk))
  )

PROMOTES
  id_ucu, id_nae, id_wof, id_zng, id_wuw, id_kxh, id_uuk

EXTENDS
  id_qog (
    (SIGMA(id_ydk, id_onc, id_awk, id_onb, id_jdd, id_vzf, id_xcf, id_phd).
     (FALSE >= "str_smw"|
      [INT, TRUE, str_qke, (MININT -->> str_eqz$0), NATURAL, str_bru$0])
     ,
     rec
       (id_glo: MAXINT,
        id_aiw: str_bla,
        id_qvk: MAXINT,
        id_gze:
        rec
          (id_sgw: str_akc$0,
           id_wqo: INTEGER,
           id_qia: str_fwn$0,
           id_wok: str_nhd,
           id_hbu: str_gzl,
           id_biv: str_uxt$0,
           id_igw: str_byp$0,
           id_vbv: TRUE),
        id_fkv:
        rec(id_fnz: str_kti, id_rag: str_qbd, id_ndv: MININT, id_olk: MAXINT),
        id_ubt: INTEGER,
        id_mwq: str_jpf,
        id_hko: str_dlm$0))
  ),
  id_gca (
    SIGMA
      (id_hpe, id_mnk, id_jyx, id_nls, id_eos, id_ike, id_muk, id_bud,
       id_xtz, id_exi, id_jzo).
    (str_lnp >= TRUE &
     not(FALSE : str_lmx$0) &
     #(id_elf, id_neh, id_oqq, id_qrf, id_jcs, id_tmp, id_oqs).
     (NATURAL /: INT)|
     str_xcg),
    (left
       (father
          (((STRING || (NATURAL |>> INTEGER)))((NAT mod str_zcp$0)'id_yte))))
  ),
  id_yoi (
    rec
      (id_ydn:
       %(id_uhe, id_lry).
       (NAT1 /= str_ykh & str_ymz$0 /<<: BOOL|
        struct
          (id_jhb: str_zpv$0, id_jae: FALSE, id_tqn: NAT, id_dwd: str_lnp$0)),
       id_lzt: (father(PI(id_kyg). (str_pwh /= NATURAL1| STRING)'id_pjs))),
    {({bool(str_obz$0 /<: FALSE), (iseq(NATURAL1))} >->>
      (dom
         (rec
            (id_agc: str_dfi$0,
             id_jmv: str_elu$0,
             id_npd: str_ppq,
             id_hio: str_ydk$0,
             id_fxv: INTEGER,
             id_ozo: str_eeg,
             id_sxx: str_hxm,
             id_ezq: INTEGER))))},
    (bool(not((MININT : str_fst => str_bxx /<: {}) & str_ogu$0 /: str_gtv$0))
       ([bool(NAT1 /<<: str_svh), str_hwm$0, NAT1]))
  ),
  id_rjo (
    struct
      (id_vhc: str_ozn,
       id_ngd:
       [[],
        rec
          (id_cok:
           INTER
             (id_ojz, id_mhm, id_yne, id_vjg, id_phz, id_srm, id_vwt, id_hqn,
              id_lym, id_syq).
           (#(id_skw, id_nrc, id_fjt, id_vij).(str_ufh /<: str_ryk$0)| MAXINT))],
       id_itp:
       { id_prc, id_jxg, id_yem, id_vci, id_egi | not(str_wfy$0 /: str_rse$0) }),
    { id_vfl, id_jzr, id_jgb, id_gag, id_qfl
      |
      !(id_xpn, id_rse).
      (!
         (id_gml, id_zda, id_ypz, id_xeu, id_kat, id_ird, id_cbf, id_xxh,
          id_nho).
       (!(id_wmc, id_dbf, id_sae, id_nfr, id_mqy).
        (#
           (id_mfi, id_ron, id_yio, id_xib, id_geh, id_rsi, id_jgo, id_pvg,
            id_wrv, id_xid, id_myg).
         ([] /: str_lki) or
         str_zoq$0 = str_dno$0)) &
       str_rqu /= str_ppf$0) }
  )

USES
  id_fvf, id_cmm, id_psl, id_oof, id_pcg, id_ntp

SETS
   id_rfr = { id_udr } ;
  
    id_dvo
    =
    { id_vpv, id_kyr, id_wxs, id_icq, id_bkg, id_udq, id_yap, id_lei }
  

CONSTANTS
  id_ens, id_how, id_hef, id_prl, id_jhn, id_jur

ABSTRACT_CONSTANTS
  id_kan, id_oqa, id_jxn

PROPERTIES
  str_xmz /<: str_fii$0 or
  NATURAL1 /<: str_bei$0 or
  #(id_wag, id_lkx).
  (#(id_ech).
   (#(id_cyv, id_ltp, id_zwc, id_xiu, id_qgf, id_yyg, id_siu, id_wzs, id_jla).
    (!(id_amq, id_cbd, id_arf, id_vgv, id_sai).(str_oyy /<: str_lfu))) or
   str_lwo <= NAT)

CONCRETE_VARIABLES
  id_fmi, id_eqn, id_quu, id_fov

VARIABLES
  id_mjo

INVARIANT
  not
    (#(id_bmo, id_gpy, id_uce, id_bvt, id_iyt).
     (NAT1 > BOOL =>
      !(id_oqy, id_gqk, id_orx).
      (!(id_wox, id_lvm, id_qsv, id_epg, id_dyo, id_bab, id_foi).
       (!
          (id_mgv, id_zrl, id_eaj, id_krc, id_and, id_whz, id_ojr, id_lcc,
           id_uay, id_wts, id_fjo).
        (!(id_shy, id_uqz, id_zrg, id_rhy).(str_pzq$0 = str_lce))))))

ASSERTIONS
  { id_mbk, id_ytd, id_vuc, id_yae
    |
    !(id_nfz, id_czq, id_obt, id_qiq, id_dyd).
    (!(id_ree, id_mpn).(NAT1 <<: str_vzv & str_cxk$0 = str_cls)) } >=
  [str_lfb, NAT1];
  #
    (id_zqv, id_zrm, id_gfj, id_opn, id_drf, id_ihw, id_dzb, id_arf, id_eir,
     id_rjf, id_oay).
  (str_tgx /: BOOL or
   (str_wyz /<<: str_try &
    MAXINT > str_dim &
    !(id_ono, id_lkn, id_nsp).
    (#(id_pbd, id_qqq, id_pgz, id_npd, id_izr, id_zxe).
     (NAT = str_agb or str_elc >= TRUE))));
  (NAT1 <<: str_pml$0 or
   (!(id_obr, id_juu, id_nwb, id_oea, id_mdo).(str_tmd$0 /= str_jst) &
    str_hic > str_ils$0)) <=>
  {str_hos$0, str_svo$0, str_aou, str_vnc$0, str_tfl$0, [], str_jre$0,
   str_yno}
    'id_ccf <
  str_fsr;
  not(BOOL <<: NATURAL <=> str_qfz$0 <= (pred(MAXINT <| INT)) |-> str_sqz$0);
  str_mma$0 =
  (str_tfw$0
     ({ id_rko, id_qar, id_ewf, id_pzp, id_tbr, id_gel, id_zor, id_rdm,
        id_kvo
        |
        #(id_dhf, id_fnw, id_uns, id_lyg, id_jyu, id_spf, id_snw, id_mej).
        (!(id_ycx, id_ckv, id_zbt, id_lcb, id_kaj, id_xyq).
         (NATURAL1 >= str_aes)) })) ><
  str_kqq$0

INITIALISATION
  ASSERT
    (str_wtc >-> (BOOL , str_xdq$0))
    |->
    INTER
      (id_vwo, id_gso, id_vxv, id_xdv, id_pvp, id_vlr, id_jrn, id_qlz, id_nqo).
    (str_mxv$0 <: str_ags| str_okr |> str_ldc) >=
    rec
      (id_urz: str_nxq$0,
       id_bpq: str_zxn,
       id_gvq: str_zaa$0,
       id_pxz: str_jqv$0,
       id_roo: str_oay,
       id_num: NAT1,
       id_vel: TRUE)
  THEN
    CASE
      bool
        ((struct
            (id_uyt: str_zku$0,
             id_ndp: str_tcf$0,
             id_lut: str_qbt$0,
             id_nke: str_gst,
             id_alx: str_lte,
             id_pzk: str_fpi$0,
             id_wte: (str_iub |-> str_neq$0),
             id_dgx: str_olj) <=
          str_zjr &
          str_zin$0 : str_ter$0) <=>
         !(id_qna, id_nqj, id_xcm).(str_bwh$0 /= str_whg$0))
      OF
      EITHER
        INTER
          (id_ndq, id_fwy, id_cow, id_com, id_xlo, id_qin, id_hyk, id_mkf,
           id_ffi, id_eee).
        (iseq
           ({ id_nxb, id_bvz, id_hui, id_gas, id_unm, id_fdw, id_jvu
              |
              FALSE /: NAT1 }) =
         str_ult|
         struct
           (id_wqk: ("str_pzi" * (str_rwy$0 /|\ STRING)),
            id_xbv: NAT1,
            id_cct: {},
            id_rdo: "str_qym",
            id_azl: str_tgk$0,
            id_msh: str_dgq$0))
      THEN
        ASSERT
          (STRING(NATURAL))
            (bool
               (!
                  (id_qhc, id_aut, id_qge, id_ifn, id_oli, id_tzv, id_gib,
                   id_wrx, id_sey, id_ckk, id_jnz).
                (not(TRUE /<: str_mec)))) /=
          str_egx$0(NATURAL)
        THEN
          id_nwy, id_msa, id_wdj, id_kyg, id_qac, id_xom, id_pwk, id_lvt,
          id_nfr :(
            #
              (id_zfh, id_lfm, id_pfi, id_ynn, id_oit, id_khi, id_fpd,
               id_ayc, id_kit).
            (not(str_iaa < FALSE <=> INTEGER >= STRING)) or
            #(id_wyf, id_qct, id_itx).(str_clj$0 /<: {str_waf, str_tpu$0})
          )
        END
      OR
        rec
          (id_kim:
           bool
             (not
                (#(id_bxt, id_rzy, id_whb, id_pcl, id_ayn, id_pth).
                 (!(id_olc, id_mna, id_wgh, id_cec).
                  (STRING , str_yto : str_pjg)))))
      THEN
        CASE
          %(id_npo, id_src).
          ({str_wsx,
            str_svd,
            {MININT, str_nda$0, str_wru, str_rmy$0, (str_sob$0 |-> [])},
            NAT1,
            str_onk,
            {[], str_eto$0, str_cds, str_edq$0, NATURAL1, str_bti},
            str_eer$0,
            str_foc$0} =
           str_xeq$0(MAXINT)|
           str_jkw$0)
          OF
          EITHER
            rec
              (id_jck: str_cit$0,
               id_fbs:
               (UNION(id_qbe, id_rzn).
                (#(id_fir, id_trv, id_omv, id_aog, id_fjb, id_flv).
                 (str_ktd$0 < str_iau)|
                 rec
                   (id_prw: str_qkp,
                    id_mzj: str_ppq$0,
                    id_zwr: str_abj$0,
                    id_taw:
                    {INTEGER, str_ffq, str_sto$0, str_cek, str_izl, MAXINT,
                     str_dxq$0},
                    id_abi: str_nii$0,
                    id_yxb: str_ccp)) -
                "str_tot"),
               id_onx: str_zxz)
          THEN
            VAR
              id_slz, id_hda
            IN
              id_iea 
                (
                  [str_iwb$0,
                   str_yxg,
                   (BOOL(str_khn$0)),
                   MAXINT,
                   str_uwt$0,
                   str_xfy]
                  |->
                  ((son(bool(not(MAXINT > str_vhq$0))))(NATURAL1))
                )
              
              :=
              {struct
                 (id_poj: str_eiw,
                  id_inu: str_ldh$0,
                  id_vmf: str_pgn$0,
                  id_cft: NAT,
                  id_lvi: str_zyd,
                  id_rtt: str_ddq,
                  id_irz: str_hql$0),
               str_ukk,
               (struct(id_mjh: str_enh)({str_eby, str_kih, NAT1})),
               str_ljf$0,
               SIGMA(id_jie). (BOOL <<: str_zgl| NAT1),
               str_mzw$0,
               str_fkx,
               str_fup$0} -
              str_omc$0
            END
          OR
            struct
              (id_lji: STRING,
               id_zgs: (rec(id_mgo: str_nso) ^ str_kxu$0),
               id_jrr:
               { id_cuz, id_csd, id_lha, id_nvc, id_ods, id_cmt
                 |
                 not(str_vuz /<: TRUE) },
               id_seg: (NAT \|/ (NAT1 +-> INT)),
               id_kms: str_phy$0,
               id_fgl: str_kdz)
          THEN
            id_sxv
              ([str_swq,
                UNION(id_jxr, id_pzw, id_tyw).
                (!(id_cri, id_ybl, id_eer, id_rwm, id_bwo, id_ght).
                 (not(str_weh$0 >= str_fkj))|
                 INTEGER),
                struct
                  (id_wtq: BOOL,
                   id_sky: NAT1,
                   id_ubv: str_wuy$0,
                   id_ooc: NATURAL,
                   id_ixi: str_khh,
                   id_pbq: str_xfg),
                (STRING(MININT)),
                str_gqm]
                 'id_cna,
               {str_zsj,
                str_jcl,
                str_cxq,
                [(TRUE , str_eck)],
                str_rym,
                (rec
                   (id_hpu:
                    struct
                      (id_nee: str_coc$0,
                       id_ezc: str_uih,
                       id_pjl: NATURAL,
                       id_dzl: FALSE,
                       id_pgm: str_kmh$0,
                       id_uqy: str_cef))
                   (str_wjq , str_hst$0))},
               (str_jtc$0'id_icl ^
                struct
                  (id_oyo: MININT,
                   id_lyy: (str_cce$0 , []),
                   id_lzz: str_iop,
                   id_asi: str_nsw,
                   id_jup:
                   { id_pwd, id_cbm, id_nkw, id_bxb | str_cwh$0 = NATURAL }
                     'id_nrr
                     'id_gah)),
               struct
                 (id_nmo:
                  { id_jho, id_bpj, id_ddo, id_crc | str_laq$0 <= {} }'id_auu,
                  id_bwi: str_zbq,
                  id_iqs: (min(str_nch , (str_eec(str_wzf)))),
                  id_khs: str_vqj'id_ctr,
                  id_qsk: str_qkm),
               (sons
                  (str_juv ..
                   PI
                     (id_gea, id_kaz, id_kju, id_psg, id_bpe, id_txl, id_gfk,
                      id_all).
                   (str_jlv >= STRING & not(str_yxp$0 >= str_dlw$0)|
                    {NATURAL1,
                     str_zgg,
                     str_hqu,
                     NATURAL,
                     { id_zpw, id_tnw, id_cbv, id_ffe, id_rqa, id_bie,
                       id_zqx, id_ytu, id_xbl, id_nnk
                       |
                       str_axw$0 > str_jmo$0 },
                     str_mhm,
                     str_lbw,
                     str_mgu}))),
               { id_mzi, id_gtp, id_vys, id_nyq, id_ims, id_jio, id_mka,
                 id_jly
                 |
                 not
                   (str_xzm$0 <= INTEGER =>
                    #(id_gii, id_bxf, id_cmx, id_lme, id_ivd, id_pmb).
                    (str_qea$0 < FALSE =>
                     !
                       (id_bup, id_oni, id_tqx, id_uej, id_zia, id_kpd,
                        id_ivl, id_bto, id_sys, id_bsi, id_vog).
                     (str_eby$0 <: str_lbx or str_yph$0 /: str_sxd$0))) },
               SIGMA(id_ssy, id_lrh, id_qhy, id_ltu, id_mwz, id_ibr, id_hrx).
               (not
                  (#(id_yet, id_ajr, id_ppa, id_ago, id_ydl, id_fnn).
                   (not
                      (#
                         (id_bem, id_wtf, id_kgp, id_mxs, id_wuk, id_afn,
                          id_ssc, id_bjx, id_rxu, id_emy, id_ugr).
                       (str_zxn /= str_ybe <=> str_kxm /<<: BOOL))))|
                ran(str_kwo$0)),
               (FIN
                  ({ id_ibb, id_uud, id_yvf, id_iqw, id_dtj, id_hon, id_ubr,
                     id_lik, id_adc
                     |
                     [] <: str_mxf$0 }
                   |->
                   (tail
                      (struct
                         (id_agj: str_ieu,
                          id_qpw: [],
                          id_pow: STRING,
                          id_kbp: INT,
                          id_rys: str_mre,
                          id_wgd: TRUE,
                          id_qrh: str_fxj,
                          id_vvc: str_bpb$0)
                       ,
                       struct
                         (id_oke: str_axz,
                          id_yck: INT,
                          id_tbo: str_kgf,
                          id_dpg: str_aqj$0))))),
               { id_hqs, id_gmm, id_yex, id_hnj
                 |
                 not
                   (not
                      (str_cht$0 >=
                       rec
                         (id_fba: str_tff,
                          id_adk: INTEGER,
                          id_hhk: str_zlm,
                          id_ttn:
                          rec
                            (id_ckt: str_wnp,
                             id_jrb: {},
                             id_rxx:
                             { id_aik, id_hso, id_uas, id_pwc, id_fhk
                               |
                               str_oit /<: str_cih$0 },
                             id_lkd: NATURAL1,
                             id_qfc: NATURAL,
                             id_pms: MININT,
                             id_tqn: NATURAL1),
                          id_avi: str_pqs$0,
                          id_srb: str_eqw$0,
                          id_nvy: str_ftr))) })
          OR
            struct
              (id_uau:
               (str_ynb'id_yqf
                ,
                {{},
                 str_jai,
                 str_nyg$0,
                 str_lqc,
                 { id_glu, id_acg, id_zcf, id_bgm, id_smi, id_pvr, id_knk
                   |
                   NATURAL > str_uvf },
                 {}}),
               id_pqu: rec(id_gun: str_mpi, id_ldn: []'id_ghq))
          THEN
            id_yfc, id_tzr, id_xml, id_ing, id_xbc, id_pue, id_yla, id_uzw,
            id_lrq, id_pkj
            :=
            (str_xoh$0((str_tof$0 |> INT)'id_nev)) **
            struct
              (id_jxk: FALSE,
               id_eml: str_ily$0,
               id_dij: (9795 |-> (-FALSE)),
               id_uvv: str_fqf$0)
          OR
            ((iseq1(TRUE))
             |->
             ({ id_sle, id_gyi, id_kup, id_mpn, id_duu
                |
                #(id_dqd).
                (#
                   (id_pdm, id_ehg, id_ten, id_dmh, id_uii, id_iac, id_fsz,
                    id_hsx).
                 (str_uue <<: NATURAL1)) }
              ,
              (MININT(str_bbr$0))))
          THEN
            PRE
              #(id_dze, id_qlu, id_gnq, id_wof, id_emg).
              (#(id_npm, id_wvh, id_owe).
               (not
                  ({(str_vcx(str_dmb)),
                    struct
                      (id_qua: MININT,
                       id_kic: str_xtv$0,
                       id_roe: str_xhe$0,
                       id_ash: str_bia$0,
                       id_idc: str_qnl$0,
                       id_zli: str_mma$0,
                       id_llo: str_wuc$0,
                       id_fzb: str_vxe$0),
                    NATURAL} =
                   str_gct$0)))
            THEN
              id_gru
              ::
              (str_odd$0'id_bzc
                 (rec
                    (id_kir:
                     SIGMA
                       (id_jkh, id_dhp, id_uae, id_olt, id_sqp, id_may,
                        id_rzx).
                     (str_qtf /<<: str_fxa$0|
                      UNION(id_bqu, id_urm, id_jmo, id_xkd, id_pje).
                      (str_ttr /<: {}| str_cft$0 , str_vgc$0)))))
                'id_elq
            END
          END
        END
      END
    END
  END ;
  id_dqv, id_vpr, id_tse, id_vmv
  ::
  struct
    (id_zua:
     struct
       (id_kvt: MININT,
        id_xtm:
        bool
          (!
             (id_hyq, id_yfn, id_xbf, id_owd, id_jmb, id_nwh, id_zzh, id_jgb,
              id_bje, id_rwe).
           (not(not(str_dpk < INTEGER))))),
     id_oen: (1 + str_cey))

OPERATIONS
  id_euq, id_aqq, id_hxj, id_fny, id_qhm, id_est, id_ukz, id_yic, id_shx
  <--
  id_rwt (id_gcq, id_oap, id_eic, id_zhf, id_vmh, id_nti, id_fvw)
  =
  IF
    not
      (#(id_zer, id_ezz, id_gip, id_veh, id_agt).
       (str_pzm$0 : NATURAL1 &
        (!(id_her, id_jnv, id_fxb, id_jlk, id_ihl).(str_mvr : str_htu) <=>
         str_lss$0 /<: str_ner)) &
       #
         (id_qit, id_slg, id_nnr, id_gdb, id_pct, id_kzo, id_dno, id_osp,
          id_wqm, id_oqj, id_lxs).
       (str_olr$0 <= str_dcr$0))
  THEN
    ANY
      id_ist, id_drw, id_cvj
    WHERE
      #(id_iyy).((str_dqk(MAXINT))(NATURAL) < str_lug$0) <=>
      !(id_qow, id_zke, id_rok, id_ksw, id_mgc, id_fan, id_ikj, id_eqn).
      (str_xqa /= TRUE <=> str_ahm$0 <<: str_vxa)
    THEN
      id_akn, id_iwe, id_vbt, id_bfn, id_euq, id_row, id_gbk, id_kdb, id_hyi :(
        !(id_spp).
        (#(id_sxu, id_xkl, id_ayu, id_qio, id_tdy, id_hro).
         (not
            (!(id_ntn, id_iye, id_ghj, id_frg).
             (#
                (id_mlz, id_ghl, id_rjk, id_udx, id_rgi, id_gjk, id_vwc,
                 id_sjc, id_pca).
              (str_uha /= str_rrr$0) &
              !(id_wwr, id_nxn, id_wuw, id_sfy, id_ybw, id_drt, id_iat).
              (str_hfh$0 /: str_jrp$0)))))
      )
    END
  ELSIF
    { id_mbw, id_xby, id_jew, id_ctu, id_qnr, id_uis
      |
      !(id_ytb, id_vfj).(str_bbs < str_jdv) } <
    bool
      (#
         (id_vws, id_pvt, id_whw, id_rjg, id_mcy, id_cgc, id_jru, id_bfu,
          id_mbi).
       (not(!(id_fzh, id_kvh, id_hld).(INT <<: str_fqm$0))))
  THEN
    SELECT
      !(id_miv, id_zhc, id_cal, id_slm, id_mrj, id_jrf, id_puc, id_mqk).
      (not
         ({ id_nja, id_pht, id_dta, id_nvq, id_hwa, id_wdm, id_eim, id_xwk,
            id_mnx
            |
            !(id_szt).
            (str_pbz$0 = str_cze <=> !(id_ptq).(str_jyo$0 <= str_iue$0)) } /<:
          str_hft$0))
    THEN
      id_fcc :(
        not
          (!(id_tyv, id_gku, id_dpk, id_fxu).
           (str_pwe$0'id_aak |-> str_dwj$0 > TRUE) &
           !(id_ldb, id_zcr, id_coz, id_uga, id_wqz, id_aru, id_wrw, id_zwk).
           (str_uwy <<: str_bun$0))
      )
    WHEN
      #(id_dcs, id_cmm, id_sit).
      (not
         (!(id_nru).
          ({ id_ckv, id_vhq, id_lkl, id_xry
             |
             #
               (id_ydp, id_ifa, id_kgv, id_kva, id_alf, id_arx, id_bwf,
                id_mtp, id_qyh, id_pcw, id_liw).
             (str_ewv$0 < TRUE) } <
           {str_jxb})))
    THEN
      id_rfp, id_tvv, id_bpu, id_dra
      <--
      id_uwy
        ((btree
            (struct
               (id_doe:
                rec
                  (id_vmo: bool(str_wwb <<: str_wsf$0),
                   id_vew: str_lrf$0,
                   id_duk: str_uyj'id_qei),
                id_fng:
                { id_gbw, id_fwl, id_new, id_lev, id_dij, id_xxz, id_rqy,
                  id_djp, id_ieg, id_hog, id_rqz
                  |
                  #(id_cbv, id_hss, id_ggi).(str_wky$0 < str_dnv) }))),
         (bool(!(id_rod, id_los, id_omy).(str_izm$0 > str_zrv$0)) +->>
          { id_jvn, id_gbe, id_tex, id_udo, id_qzf
            |
            !(id_osf, id_lrf, id_jvw, id_fps, id_cfh, id_luu, id_uww, id_vtz).
            (#(id_dob, id_tva).(str_ush$0 /<<: str_bzy$0)) })
           'id_crn,
         (((succ(MAXINT)) ><
           %(id_elq, id_hxi, id_lxo). (NATURAL1 /: NAT| str_alu))
            ([str_upc,
              str_qta$0,
              struct
                (id_oet: str_hnm,
                 id_hpq: NATURAL,
                 id_orb: str_xnf$0,
                 id_hdy: str_tgu,
                 id_efs: str_ibr,
                 id_ayc: str_vrw$0,
                 id_myd: (str_hob$0 |-> INTEGER),
                 id_fjq: STRING),
              str_prs$0,
              str_qmn,
              str_nne])),
         ({{ id_dgr, id_dde, id_jjv, id_tdf, id_qrb, id_vxa
             |
             not(str_sgu$0 : str_jbb <=> INT : str_atf) }} mod
          rec
            (id_nlb: "str_quj",
             id_pfy: str_hrl,
             id_wbx: str_vpz,
             id_lpo: str_jae$0)),
         rec
           (id_rzi: FALSE,
            id_duh: str_iwn,
            id_uci:
            {str_pgr,
             [str_out$0, NAT1, str_ehf$0, str_vzu$0, str_tas],
             str_cnd$0,
             str_dnu,
             str_cbq,
             INT,
             { id_tzn, id_ztl, id_ucj, id_rvn, id_ooe, id_mnb, id_zuw,
               id_bkm, id_uxk, id_jbh
               |
               NAT1 = str_xkm$0 => [] <: str_gwb$0 }},
            id_rus: str_otp),
         {str_mlb,
          struct
            (id_rzu: str_jzt,
             id_nqg:
             bool
               (not
                  (#(id_dbm, id_aly, id_npk, id_cxu, id_eat, id_khk).
                   ({} : str_slk$0))),
             id_uqt: str_pnj,
             id_pyd:
             rec
               (id_fgl: FALSE,
                id_crp: str_yfc,
                id_rhl: BOOL,
                id_lvk: NATURAL,
                id_yst: str_xed$0,
                id_dva: MAXINT'id_gvd,
                id_bpc: INT)),
          "str_and",
          553,
          str_fkx},
         (INTER(id_yzy).
          (not(str_mmi$0 /<: MAXINT & [] >= str_itx$0)|
           UNION(id_dor, id_sjg, id_mkh, id_jkn, id_bku, id_xim).
           (str_wlb /<<: NAT| NAT1)) +->
          [str_vma, str_wmt$0, str_dww, str_yku$0, str_bri, NATURAL,
           str_zec$0, TRUE]
            'id_tcv),
         {%(id_qsg, id_xme, id_obm).
          (INTEGER /<: str_acn|
           { id_jzg, id_cgf, id_ogo, id_wsh, id_hrf, id_ldr, id_bks, id_dqw,
             id_whx, id_hbo, id_tuu
             |
             BOOL > str_xqb }),
          [str_hst$0,
           INT,
           (father
              (struct(id_pvq: INTEGER, id_mpw: str_pnp, id_nho: str_mse$0))),
           NATURAL,
           NAT]
            'id_vey},
         bool
           (#(id_qvw, id_drp, id_thj, id_qow, id_obn).
            (str_vqz$0 <= str_hmp =>
             struct
               (id_sre: {},
                id_dbc: NATURAL,
                id_soi: str_ppm$0,
                id_kir: NAT,
                id_lqv: NATURAL,
                id_gbg: str_avh$0,
                id_nuj: 0,
                id_iki: NATURAL)
             ,
             str_umk /=
             closure(MININT))),
         PI(id_tsh, id_anh, id_cyb).
         (#(id_xsk, id_hqu).
          ((#(id_rfi, id_igb, id_nkb, id_eql, id_xly, id_pzv, id_dwb, id_uiv).
            (FALSE <<: str_yhd$0) =>
            str_fmn <<: str_lat) &
           MININT /= INTEGER)|
          { id_jwa, id_ore, id_yum
            |
            str_cvc /: MAXINT => str_gfq <<: str_sji$0 }))
    WHEN
      not
        (not
           (#
              (id_ozx, id_qln, id_caq, id_das, id_boz, id_oow, id_esc,
               id_gvm, id_ogm, id_fee, id_bhp).
            (INT >= str_ahh$0 & str_njz /<<: str_jgh))) &
      (#(id_wgv, id_nxr).(NATURAL > str_goq$0) <=> str_fce$0 <= STRING)
    THEN
      id_aox, id_djo, id_rnl :(
        #(id_und).
        (NATURAL >
         rec
           (id_klm: (last(NATURAL1)),
            id_wce: ("str_toc" |-> str_pfz$0),
            id_jcd:
            struct
              (id_qqq: str_jkl,
               id_znk: str_lqj,
               id_dvb: str_hie$0,
               id_ayu: MININT,
               id_lmo: FALSE,
               id_ogu: str_feu,
               id_fbu: MAXINT),
            id_vrj: str_jzl$0,
            id_lcs: (NAT1 .. STRING)))
      )
    WHEN
      #(id_jsz, id_bmi, id_hos, id_ean, id_qko, id_xox, id_sai, id_oxs).
      (not
         (#
            (id_usv, id_ibn, id_sve, id_mei, id_elg, id_fsx, id_pgp, id_wln,
             id_gco, id_yzb).
          (not(str_vak <= last(str_kyn))))) &
      NAT <<: str_aai
    THEN
      LET
        id_pfj, id_dxs, id_ilh, id_tvd, id_cut, id_zkg, id_ebt, id_agz,
        id_ycr, id_ojh, id_pdl
      BE
        id_wkt
        =
        (bool(str_dph$0 < str_nzz$0 & !(id_pim, id_qdr).(MAXINT < str_xjj$0))
         ,
         (str_dxe$0
          |->
          INTER(id_gtv, id_xul, id_juz, id_nzg, id_ore, id_blf).
          (str_gxl /= str_xvc| str_axk)))
          'id_mzt&
        id_bvi
        =
        rec
          (id_faz:
           rec
             (id_tyz: TRUE,
              id_ffs:
              bool
                (#(id_vka, id_uii, id_ldw, id_nvs, id_ocb, id_yoi, id_qww).
                 (#(id_yay, id_jjq, id_aox, id_fxx).
                  (#(id_uvk, id_qyc).(str_iuk /: MAXINT)))),
              id_lro: FALSE),
           id_mqu: str_yjq$0,
           id_rkt: str_fid$0,
           id_pha: str_rod$0,
           id_xkv: NAT1,
           id_qbo: rec(id_rjc: str_ksi$0))&
        id_ebp
        =
        PI(id_ohw, id_afl, id_wzo, id_csi).
        (!(id_myf, id_ube, id_auo, id_vmi, id_czf, id_vle).
         (#
            (id_nxn, id_yki, id_jdh, id_znn, id_uce, id_crt, id_zow, id_axo,
             id_svf).
          (str_wvu >= str_mnc$0))|
         INTER(id_lsq, id_qpp, id_yzt, id_kvx).
         (!(id_qlv, id_gbk).(str_wfc /<<: STRING)| str_jzx$0)
           'id_oak mod
         str_msi$0)&
        id_lwa
        =
        rec
          (id_ekx: BOOL,
           id_cla: MAXINT,
           id_ntr: str_exd,
           id_xyf:
           [bool(str_gtq$0 < str_ftg),
            str_ccy,
            (postfix
               (SIGMA(id_nox, id_oww, id_tvu, id_ubt).
                (str_tzv$0 /: str_cgg or
                 #
                   (id_ygr, id_ejq, id_osh, id_zja, id_zks, id_xnk, id_ooi,
                    id_ana).
                 (str_ynv > str_anj)|
                 INTEGER))),
            str_bnl$0,
            str_xym$0,
            str_unj$0,
            str_yvu,
            str_ido],
           id_fya: MININT)&
        id_xxf
        =
        rec
          (id_wfp: str_skx,
           id_ppg: NATURAL1,
           id_lwq:
           { id_lpp, id_srq
             |
             #(id_sac, id_bbk, id_crr, id_yxy).
             (!(id_xur, id_zmz, id_fjl).
              (!(id_jap, id_wxb).
               (str_cig <= NATURAL or str_bxz$0 > str_mlr$0))) })&
        id_hbi
        =
        rec
          (id_gfn:
           rec
             (id_toc: str_jlf,
              id_hro: FALSE,
              id_bqj: str_evk$0,
              id_itc: {},
              id_pbq: str_hqu,
              id_hiu: str_xtj$0,
              id_rms: {},
              id_bmw: INT),
           id_mnm:
           UNION
             (id_glm, id_dmx, id_mbo, id_rbg, id_zan, id_oea, id_fcx, id_qdc,
              id_szc, id_zfd, id_xlp).
           (#(id_gok, id_slq).(str_pym$0 /<<: str_ojp)|
            str_bqk$0
              (INTER(id_avj, id_ulc, id_cii, id_yvo, id_qdj, id_mwl).
               (str_imf >= str_fly$0| str_zyi$0))),
           id_zll: str_npn$0,
           id_ixe: (str_mvo |-> str_qjh$0))&
        id_snn
        =
        {FALSE,
         MAXINT,
         FALSE,
         { id_dqs, id_ctb, id_yme, id_ehp, id_hff, id_xhh, id_mlx, id_ndg,
           id_yjv, id_zhg, id_jmd
           |
           str_tta >= str_rve$0 },
         INTEGER,
         str_adm} ->
        {str_vby,
         NATURAL1'id_fjp,
         FALSE,
         (NAT1 mod str_ebu),
         bool(str_vnc$0 = str_ogb),
         str_rsp$0,
         str_zrm}&
        id_qlq
        =
        bool
          (!
             (id_xqr, id_mvq, id_ptf, id_msd, id_hsi, id_nzw, id_mef, id_tlj,
              id_kxm).
           (not
              (str_cqe$0 /<<: {} =>
               !
                 (id_usc, id_wni, id_dqu, id_cqv, id_zvr, id_ktv, id_mjr,
                  id_hrr, id_lkd).
               (not(#(id_tnz, id_gdx, id_urz).(STRING <= str_cyy))))))&
        id_xdt
        =
        rec
          (id_xpk:
           (tree
              (struct
                 (id_njb:
                  {str_vpl$0,
                   PI(id_apo, id_fnx, id_tro, id_rfr, id_nff).
                   (str_tjb$0 /= str_wlf|
                    SIGMA
                      (id_lni, id_com, id_xtc, id_soj, id_wdc, id_jqm, id_tga).
                    (str_bwv$0 <<: STRING| str_hsp)),
                   str_olk$0,
                   []},
                  id_yeu: str_ueg,
                  id_pud: str_hnn)))
             'id_uth,
           id_eoj: str_rmm,
           id_xdz: str_exg)&
        id_xig
        =
        {(FALSE([])),
         str_ejo,
         str_yvl$0,
         ((POW1(str_zpr)) <-
          ([INTER
              (id_iqw, id_mkz, id_kaq, id_wap, id_wes, id_xmz, id_coc,
               id_lpc, id_zsl).
            (FALSE >= INTEGER| str_nab$0),
            MININT,
            BOOL,
            str_zhp,
            [],
            str_gxc$0,
            str_vms$0,
            NAT] -->
           str_gnk$0))}&
        id_dcg
        =
        bool
          ((str_hzz > NATURAL <=>
            str_djk$0 /:
            rec
              (id_pij: str_aua$0,
               id_ssl: BOOL,
               id_dvi: str_gkk,
               id_quu: str_ior$0,
               id_thu: NATURAL,
               id_gkq: str_lih$0,
               id_dwg: {})) &
           str_ste < str_lju$0(NATURAL1))
      IN
        skip
      END
    ELSE
      id_bnq 
        (
          [struct
             (id_mvn: NATURAL1'id_ddn,
              id_caf: [(left(str_abo)), str_uxi$0, str_hmz$0'id_emf]),
           rec(id_eew: MAXINT)]
        )
      
      :=
      struct
        (id_vyw: (str_uki(NAT1)),
         id_vbh: FALSE,
         id_pqt: NATURAL1,
         id_qje:
         (bool(str_ufa$0 /= str_yow)
          ,
          rec
            (id_yja: str_pym$0,
             id_zaa: BOOL,
             id_sjq: str_npz$0,
             id_ucr: INTEGER,
             id_lfx: (FALSE |-> (str_xug$0(str_ddm))),
             id_dtn: STRING,
             id_met: str_qbz$0,
             id_cro: NAT1)),
         id_pdn: str_ndx)
    END ;
    BEGIN
      SELECT
        INT <: str_mbg$0 =>
        #(id_ogn, id_qej, id_zqc, id_mlt, id_epd, id_qhi).
        (!
           (id_nil, id_kxf, id_mvd, id_wjf, id_rkm, id_fat, id_yqt, id_qfm,
            id_pwy, id_ojv, id_zxt).
         (!
            (id_woy, id_kkt, id_oed, id_mtv, id_gql, id_zil, id_hxy, id_crd,
             id_clt, id_uls, id_pto).
          ((str_nml$0 : str_kxx$0 & str_lwb$0 = str_zjn) or
           str_yoy$0 >= str_dju$0)) &
         str_lyt >= BOOL)
      THEN
        id_kmn, id_cyk, id_eit
        ::
        [SIGMA(id_twg, id_umy). (str_std$0 < INT| str_lup$0),
         (str_auq , NAT),
         (str_chh +-> MAXINT),
         FALSE'id_tim]
        |->
        {NATURAL, NATURAL}
      WHEN
        !
          (id_veb, id_cly, id_inz, id_fna, id_pyc, id_cyd, id_yyx, id_nhi,
           id_jil, id_bov, id_wow).
        ({{str_cnu, NAT1, str_kvn, NAT, str_toa},
          rec
            (id_wjt: NAT,
             id_jjn: str_tvz$0,
             id_jpi: str_uel,
             id_nuw: str_jet$0),
          str_rpd,
          UNION
            (id_ubi, id_sxj, id_pyt, id_nhg, id_whc, id_jhi, id_zlt, id_dqe,
             id_opn).
          (str_cfj$0 /: str_wsp| []),
          NATURAL,
          BOOL} >=
         str_kar or
         BOOL <= str_atr$0)
      THEN
        skip
      ELSE
        skip
      END ||
      id_vkl, id_ndn, id_xpf, id_urn, id_prd, id_xps, id_vuk
      ::
      {STRING,
       struct
         (id_nqr: {str_gqk$0, str_fqo$0, str_oce$0, str_xor, str_fvn},
          id_acl: str_kjh$0,
          id_bfd: str_dwo),
       [(str_nsd$0 <+
         UNION
           (id_kcw, id_yak, id_fgt, id_bhq, id_zxx, id_elj, id_wbh, id_kyt,
            id_xgv, id_sje, id_qfr).
         (not(str_uaa < str_thb$0)| str_edj$0)),
        str_sgn$0]}
    END
  ELSIF
    { id_edv, id_mye, id_esk, id_mjl, id_eqh, id_xrx, id_jlt, id_ohn
      |
      str_gjk /: str_jpx$0 } <
    { id_qtz, id_yil, id_vwv, id_ijp, id_hfb, id_zul, id_hnw, id_wvt, id_gtx,
      id_yoe
      |
      !
        (id_tfz, id_naq, id_nub, id_zgp, id_nqk, id_kbx, id_jph, id_yuk,
         id_kgl, id_ljg, id_uoo).
      (!(id_vxb, id_iri, id_nma, id_foi).
       (#
          (id_wag, id_xrn, id_xwr, id_uvy, id_ovu, id_nyj, id_yug, id_fuv,
           id_bqg, id_iiy).
        (str_pvc$0 <= str_dxc))) }
      'id_wge
  THEN
    id_gso, id_oxw, id_ovi, id_ukw, id_ngl, id_bal
    <--
    id_sae
      ({ id_ivs, id_paz, id_mnr, id_zoo, id_nmw, id_yei, id_ybc, id_zyu,
         id_rys, id_oyf, id_cws
         |
         not
           (str_tdp :
            [FALSE,
             str_dnz,
             UNION(id_jvy, id_ynj, id_ahp, id_idp, id_lje, id_uix, id_qbs).
             (str_jbj$0 <<: MININT| str_qzu$0),
             (bin(str_qaa$0~)),
             TRUE,
             {}]) },
       SIGMA(id_vbe, id_twx, id_sdp, id_wbr, id_wph).
       (not(str_mmg /: {})|
        MININT
        ,
        struct
          (id_xjo: str_vri$0,
           id_yhr: str_eqw$0,
           id_qby: str_cvd,
           id_klg: str_btq$0,
           id_muj: NAT1,
           id_vvm:
           rec
             (id_dyb: str_utw,
              id_hss: NAT,
              id_jia:
              {{}, str_qqh, NATURAL1, str_nod$0, str_exi, TRUE, str_ktc,
               MININT},
              id_hbx: INTEGER),
           id_pru: FALSE,
           id_wuj: str_jmh))
         'id_fyb,
       bool
         (#
            (id_seb, id_iig, id_jcx, id_zji, id_dvs, id_hai, id_vub, id_tlx,
             id_icu).
          (struct
             (id_pcp: str_ajr,
              id_ikj: MININT,
              id_gpy: str_gfg,
              id_bgg: NAT,
              id_egs: str_hxb,
              id_fjz: str_ehj$0,
              id_pcj: bool(FALSE = str_tan),
              id_jcd: {MAXINT, {}, str_plr$0, NATURAL1, str_wrw$0, str_awq}) <<:
           str_urc))
         'id_ghe,
       (SIGMA(id_amm, id_dlm, id_ndz, id_tod). (str_dhc$0 <= INT| -MAXINT)
          ((left(str_kag)) mod rec(id_edc: str_epv)'id_hcl)),
       INTER(id_cxi).
       (not(not(BOOL < str_cvp)) =>
        #
          (id_qws, id_dmn, id_gmu, id_feo, id_cja, id_olg, id_boz, id_nyk,
           id_iwz).
        (#(id_jfi, id_fqk, id_gsu).({} /= str_jmq))|
        NAT(str_lbf$0)))
  ELSIF
    !(id_nop, id_yon, id_eod).
    (!(id_guw, id_ytp, id_vej).
     (#(id_bgb, id_htb, id_zyi, id_rrs).
      (NAT /<: str_moc$0 & not([FALSE, [], NATURAL, str_umt] = FALSE))))
  THEN
    id_svf, id_kih, id_dnc, id_dhb
    :=
    prefix
      ([BOOL,
        SIGMA(id_rrl, id_dqh, id_sym, id_nlj).
        (str_hzx = NATURAL1| str_mcl$0),
        str_wwu$0,
        str_vip,
        TRUE,
        ((str_mdy <<| str_pjv)
         |->
         rec
           (id_pmy: str_npa,
            id_ewd: str_rvf$0,
            id_uei: str_btr,
            id_xjt: struct(id_hcx: TRUE)))])
  ELSIF
    !(id_oom, id_ehx, id_dae, id_osi, id_vmh, id_fqi, id_bcz, id_daj, id_eqf).
    (!(id_zyv).
     (#(id_mbv, id_qjd, id_odi, id_grt, id_edd, id_duc, id_yux).
      (#(id_fjh, id_nza, id_szp, id_jle, id_gsa).(#(id_hco).(INT = str_vby))) =>
      str_bki$0 /: str_quc$0))
  THEN
    id_dqr
    ::
    {rec
       (id_jdq:
        struct
          (id_xmq: str_byd, id_pwp: BOOL, id_bom: str_wmz$0, id_oql: MININT),
        id_obu: bool(!(id_xwp).(str_kbv /<: "str_pxs")),
        id_qbt: (str_gkn$0 mod str_eqv$0))}
      'id_mtm
  ELSIF
    !
      (id_eki, id_mnb, id_tms, id_gfi, id_rht, id_cfo, id_hxs, id_fft,
       id_huw, id_qfn).
    (not
       (not
          (!(id_yla, id_uqz, id_add, id_wrx, id_nhn, id_iba).
           (INT /: str_cae & !(id_jwk, id_bpc).(str_bsb$0 <<: str_gpi)))))
  THEN
    id_vwy, id_zcq <-- id_hcq
  ELSIF
    #(id_bri, id_dmg, id_wjc, id_zbu).
    (#(id_rqq, id_sbw, id_loh, id_jta).(!(id_gko).(str_vry$0 < str_xmx$0)) <=>
     str_aws <
     INTER
       (id_sxw, id_fpm, id_hux, id_kcq, id_wjl, id_pjs, id_poa, id_sni,
        id_eat).
     (str_ktq$0 /= str_tob| str_lzo'id_tup))
  THEN
    id_qci, id_lmq, id_rsq, id_aok, id_jug :(
      NATURAL <= str_bop$0 =>
      rec
        (id_qjw: str_qmd,
         id_znc: NATURAL,
         id_zxx:
         (FIN1
            (rec
               (id_gna: FALSE,
                id_zeg: STRING,
                id_htc: NAT,
                id_wth: 8,
                id_yaz: STRING,
                id_jfa: str_gaf$0,
                id_zku: NATURAL,
                id_xeo: str_wdn)))) =
      rec
        (id_zpa: str_tpv, id_ily: str_zhn, id_nfh: struct(id_vcy: str_dds$0))
    )
  ELSE
    id_ema 
      (
        {str_lpz,
         struct
           (id_acr:
            ([str_qpo, str_qkl, str_hfu$0, str_ggu, NATURAL1, str_eel, MAXINT] >->>
             str_zeo),
            id_ryk: ((str_are$0 ; NATURAL)),
            id_rwn: str_wwa,
            id_snm: INT,
            id_mjx: str_vuv$0),
         str_kzn,
         (({} ; struct(id_mct: str_std, id_tlp: str_zdk$0)))}
      )
      (
        {str_pki,
         ((perm(bool(str_lrt <<: str_jgi)'id_cin))
            ({str_kye$0, str_atu, str_yso, str_fer, NATURAL, str_haf,
              str_spu, str_xhn$0})),
         TRUE,
         NAT1,
         NATURAL,
         str_peb$0,
         str_kez}
      )
      (
        left
          (struct
             (id_pxk:
              ([str_cws, str_ezm, str_rmx, str_ksc, MININT, str_hfp$0,
                str_zhy]
               |->
               (str_qyd(MAXINT))),
              id_cpr: PI(id_nsx, id_jka). (str_qao <: str_diy| str_ppy),
              id_aql: bool(str_udf$0 > str_rrl$0)))
      )
      (
        [str_dny,
         str_uqn$0,
         str_zuc,
         str_bed,
         str_wlh$0,
         str_trr$0,
         str_gkj,
         bool
           (!
              (id_lno, id_nvn, id_ixw, id_bhi, id_wzr, id_sav, id_aks,
               id_wpa, id_nqk, id_cnh, id_udz).
            (NAT <<: str_iso) <=>
            (#
               (id_cuf, id_ngf, id_vgs, id_dvh, id_gnf, id_gzd, id_wny, id_ejd).
             (str_syx : NATURAL1) <=>
             {} <: str_ybe$0))]
      )
    
    :=
    { id_ccl, id_jsw
      |
      dom(str_jht$0) <<: {} or
      (#(id_pll, id_wvu, id_uzo, id_lqz, id_hfr, id_poh, id_qpu).
       (MAXINT <<: str_ifq) =>
       str_xrr$0 < STRING) }
  END;

  id_our, id_uds, id_mdr, id_nxc, id_eza, id_wda, id_qqg
  <--
  id_txe (id_eko, id_bpp, id_hyr, id_tbg, id_kds, id_ach)
  =
  CHOICE
    id_azu'id_tex
    :=
    (((str_ywg .. (str_kmd$0 |-> str_sup$0)) + str_xoq) |-> str_nqi)
      (str_xna$0 , {str_huu, str_kmt$0, str_qqg, str_yrz, BOOL, str_icl})
  OR
    VAR
      id_loj, id_gco, id_jed, id_wux, id_jom, id_oth, id_hif, id_kmt, id_ipt,
      id_zxz
    IN
      skip
    END ;
    id_eno 
      (
        [str_hpu,
         [str_yhm$0,
          str_kuu$0,
          rec
            (id_lqq: str_uqh$0,
             id_pkg: "str_dsp",
             id_opz: str_hvs,
             id_mfa: str_sct$0,
             id_qcy: MAXINT)
            'id_amk,
          str_jdb,
          str_yev$0,
          (str_aep$0 .. str_rbv$0)],
         (MAXINT(str_gmd$0))]
          'id_npd
      )
      (
        {{str_wdt,
          struct
            (id_klf: str_izd$0,
             id_jot: str_rmn$0,
             id_hob: INT,
             id_jll: str_qbg$0)
            'id_tld,
          str_ivl},
         (struct(id_fho: str_wfa$0, id_och: FALSE)(str_pba)),
         BOOL,
         (str_mof$0 , str_gum$0)}
      )
      (
        { id_rju, id_aus, id_bvh, id_nrn, id_oxi, id_nyw, id_rey, id_rfx,
          id_lkt, id_osb, id_gdl
          |
          not
            (not
               (#
                  (id_pvb, id_jvn, id_bdn, id_vjb, id_fue, id_suc, id_ydb,
                   id_thl).
                (!(id_bom, id_vrp, id_skn, id_wqc, id_kel, id_vzk).
                 (str_xki$0 >= MAXINT =>
                  (str_ypt >= str_ckm & str_wip > STRING))))) }
      )
      (
        { id_sam, id_lao, id_cjd, id_hed | INT /: NAT }
        |->
        (str_vpo
         |->
         struct
           (id_gcu:
            (rec
               (id_uue: TRUE,
                id_rhs: (-str_ehl),
                id_cft: str_zia,
                id_mgx: str_wrj$0)
               [str_kti$0])))
      )
      (
        rec
          (id_bvi:
           { id_jep, id_dmr, id_cpp, id_eii, id_wcj, id_zte, id_lts, id_rfj,
             id_exx
             |
             str_djn < str_mhf$0 or str_dos > str_wmp$0 },
           id_qaa: (FALSE(str_mhe$0)),
           id_jpv: [str_ktb$0, (str_wem(str_giq$0)), INT, str_sxc$0],
           id_inq: str_gqv$0,
           id_xip: (str_mrb \/ []))
      )
      (
        {[],
         ((str_wuz$0(str_iga$0'id_dku)) * NATURAL1),
         str_qby$0,
         str_bem$0,
         PI
           (id_xsf, id_nbs, id_qwz, id_jnm, id_qxj, id_lju, id_kah, id_myw,
            id_oha, id_jvy).
         (not
            (!
               (id_vxi, id_iay, id_whx, id_fgo, id_xwn, id_nhu, id_oey,
                id_hpq, id_pml).
             (FALSE /<<: TRUE))|
          str_cpu),
         str_muu,
         str_uls$0,
         str_lik}
      )
    
    :=
    (iterate
       ({str_bes$0,
         str_ffk,
         "str_idi",
         bool
           (!
              (id_fys, id_pac, id_oui, id_ypo, id_oaj, id_lik, id_bhs,
               id_vdv, id_znp, id_nxc).
            (str_vym$0 /<<: str_kyt$0)),
         str_kqh,
         str_nag$0,
         str_ekt,
         str_smj$0}
        ,
        str_qmf))
    ,
    str_cxg$0
  OR
    ASSERT
      #
        (id_bym, id_tlg, id_lok, id_cdb, id_kne, id_heh, id_ihf, id_olv,
         id_pdt, id_vzm).
      (#(id_rdz, id_pwt, id_ris, id_lsh, id_ckx).
       (#(id_jec, id_uaq).
        (!(id_gkv, id_fzb, id_vmy, id_wjl, id_eaj).
         (not(#(id_ikx).(str_buo /: str_pxm$0)))))) &
      NATURAL1 < str_xtr$0
    THEN
      id_mqj'id_kvo
      :=
      rec
        (id_aye:
         [rec
            (id_oac: str_ofv$0,
             id_lrl: str_ysg$0,
             id_bgf: INTEGER,
             id_dtw: str_ipq$0),
          [],
          INTER(id_tzs, id_roa). (str_vtd /= INTEGER| str_wwu$0),
          str_jxn$0,
          str_yfj$0,
          str_efc,
          str_ctt,
          [NATURAL, str_diz, str_nby]],
         id_dba: (top(bool(str_hbw /<<: NAT1)))) ;
      id_fum, id_rqb, id_dsm, id_tqz, id_ywt, id_zoz, id_ncg
      <--
      id_ygr
        (bool
           (#
              (id_xzo, id_wgw, id_llh, id_edz, id_okg, id_dwr, id_yxo,
               id_qcs, id_yem, id_oyb).
            (#(id_bso, id_gqf, id_xnb, id_qmr, id_gmp, id_and).
             (str_mok$0 /= NATURAL1'id_cwx)) or
            not(str_nhd /<<: str_aam)),
         (top
            ([MAXINT,
              NAT1,
              PI(id_bac, id_hxw, id_upb, id_tof, id_tvr, id_cox, id_eql).
              (str_dib$0 /= str_tzk|
               {{ id_bfv, id_uml, id_gif, id_wox, id_aix, id_eww, id_tfs,
                  id_iop, id_fcw, id_tgc
                  |
                  STRING /= str_ygs$0 }} /|\
               (right(NATURAL1))),
              str_asi$0,
              str_xwi$0])),
         { id_tje, id_ebd, id_ubm, id_xqo, id_nsu, id_xgr, id_tce, id_ofb,
           id_ecc, id_kcb, id_oem
           |
           (!(id_dfr).(str_ccz <: NATURAL) <=>
            (NATURAL < NAT1 &
             (str_ixo /: "str_kpk" => str_via$0 >= str_mbu$0))) =>
           str_uoo <: MININT },
         INTER
           (id_vnh, id_wvd, id_opa, id_eaz, id_jjd, id_wqz, id_fef, id_yjl,
            id_vkj, id_efu, id_bba).
         (UNION(id_qgp). (str_tkj$0 /<<: str_eez$0| INTEGER) /<<:
          rec
            (id_otq: str_mxk,
             id_fjp: str_bcs,
             id_ojs: NAT1,
             id_okv: TRUE,
             id_xty: str_aka,
             id_qbp: [],
             id_qvu: str_oox,
             id_ydu: str_afm)|
          PI(id_ckt, id_laz, id_bsv, id_tfm, id_gye, id_mso).
          (str_lfb$0 /<: INT| str_hzp$0 <-> bool(str_ija$0 >= str_dow))),
         { id_ogf
           |
           #(id_qpf, id_kwy, id_bum, id_sqd, id_rqo, id_icp, id_cfb, id_apd).
           (not
              (iterate(sizet(str_zen)) /:
               { id_fpd, id_cwi | str_kto /: str_fya$0 })) },
         { id_lyx, id_ojn, id_psw, id_gun, id_zud, id_gfs, id_get
           |
           #(id_rhg, id_zra, id_fho, id_fsa).
           (#
              (id_rnn, id_xee, id_vaz, id_umh, id_cxi, id_gub, id_jkl,
               id_mpk, id_tan, id_oul, id_itj).
            (not
               (UNION(id_hfn, id_ocp, id_wqd, id_htu, id_cql, id_xlp).
                (str_qxg > NAT1| NATURAL1) /=
                str_pdt'id_efx))) },
         bool
           (str_ggw < NATURAL or
            (BOOL /= str_yve <=>
             !(id_lhb).
             (#
                (id_qlc, id_pzz, id_acj, id_tss, id_qta, id_jjx, id_vps,
                 id_eur, id_zkq).
              (not
                 (!
                    (id_swi, id_mpt, id_eft, id_mxp, id_gfw, id_stk, id_ogp,
                     id_rtt).
                  (str_usp >= str_jtl$0)))))),
         UNION(id_yis, id_ywd, id_pze, id_azt, id_gpw, id_caj).
         ([str_bxo$0,
           str_uqg,
           str_spl$0,
           str_yvt,
           str_eii,
           [STRING,
            str_sdi$0,
            str_ekf,
            str_bcf$0,
            NAT1,
            [],
            (str_nol$0 / str_olw$0)],
           str_viv$0,
           MAXINT] <=
          str_eow$0|
          %(id_eda, id_syj, id_krh, id_jse, id_pog, id_itu, id_cqy).
          (str_ljc$0 >= str_fcq$0 <=> str_leg <: str_cmq| str_dac$0)))
    END
  OR
    ASSERT
      !(id_uji, id_pce, id_qpx, id_vwu).
      (str_bzn >=
       bool
         ([str_vsw$0, [], str_zlt$0, str_rux, str_nlr, NATURAL, str_jkr] /:
          BOOL'id_ujv or
          str_rai /<: MININT))
    THEN
      id_xcg 
        (
          {TRUE,
           FALSE,
           UNION(id_jvo, id_kuv, id_ute, id_rrk, id_vgr, id_fxp, id_ubf).
           (POW(str_etz) =
            struct
              (id_eid: MAXINT,
               id_brz: str_wkr,
               id_wsj: str_xlq$0,
               id_rgy: str_jzq,
               id_isl: {},
               id_hum: [],
               id_iop: MININT)|
            1(str_mtp$0))
             'id_yuk,
           str_lrp}
        )
        (
          ((FIN(str_pgj$0))
             (rec
                (id_ovt: str_php,
                 id_jge: str_pqc,
                 id_dlp: str_eus,
                 id_wpm: str_fbp$0,
                 id_wjv:
                 [[], str_hkd, FALSE, str_lyt, [], str_krn$0, str_qeq$0,
                  str_qya])))
          ,
          rec(id_ssz: (rev(MININT)))
        )
        (
          rec
            (id_mhx:
             [str_qon,
              NATURAL,
              %(id_vtg, id_hvc, id_ljo, id_gmn, id_rsh).
              (!
                 (id_kpy, id_eux, id_rlv, id_jsa, id_awq, id_xmw, id_tom,
                  id_inw, id_rzy, id_xpa, id_hwx).
               (NATURAL1 < {})|
               rec(id_oag: str_rnf$0, id_bya: str_hud$0)),
              str_peh],
             id_fhe: INTEGER,
             id_cdc: str_knw,
             id_qfl: str_agc,
             id_wkf:
             struct
               (id_ray: NATURAL1,
                id_crz: str_lyg$0,
                id_jui: str_dlc$0,
                id_cew: INT,
                id_han: str_mhm),
             id_dfu: {str_vuj$0, TRUE, str_tam, NAT1})
        )
        (
          { id_mff, id_mas, id_afe, id_byp, id_rbm, id_rxf, id_cqr, id_fyy,
            id_odr, id_hwi, id_yab
            |
            #(id_ofh).
            (!(id_xoq, id_bph, id_jdi, id_hzf, id_ire).
             (!
                (id_ytk, id_jqd, id_zdk, id_sup, id_yts, id_jes, id_okw,
                 id_wpw, id_dbo, id_ikg).
              (not
                 (!
                    (id_ilo, id_syh, id_yvb, id_xtg, id_yoh, id_txc, id_hfa,
                     id_xdo).
                  (str_pnm /<: str_gfh))))) }
        )
        (
          %(id_aae).
          ({} -
           (infix
              ([rec
                  (id_crs: str_mrf$0,
                   id_zhx: str_tdo$0,
                   id_tlq: TRUE,
                   id_prx: str_omc,
                   id_qkw: str_pqf$0,
                   id_npy: STRING,
                   id_xkc: str_pcz$0,
                   id_gqr: str_qkb),
                str_ygd$0,
                str_heq$0,
                str_dme,
                str_xji,
                str_mpz,
                str_wxx,
                str_znb])) =
           MAXINT|
           { id_umf, id_rzw, id_pqs | NATURAL = str_zci$0 })
        )
      
      :=
      struct
        (id_xlb: str_ksc$0,
         id_wqe: BOOL,
         id_bvg: str_nfz,
         id_ypk: str_xmo,
         id_hpz: INTEGER,
         id_juq:
         [NAT, (((str_qho$0 \/ str_ibd) +->> str_fwa$0'id_wdi) |-> "str_rtr")],
         id_mbs:
         SIGMA(id_jyh, id_pxf, id_gqr, id_tvs, id_lqb).
         (INT /= str_tji$0| str_qrj$0))
    END
  OR
    IF
      { id_ezl, id_mhc, id_mmr, id_bzu, id_idt
        |
        #(id_wii, id_nvc, id_sbz, id_itt, id_pss).
        (#(id_bcm, id_eat, id_uls).
         (#(id_fdu, id_aiw, id_oie, id_nba, id_bjv, id_lta, id_gjz).
          (str_wno$0 >= INT)) =>
         (str_axv /<: str_one <=> str_aib$0 : [])) } <=
      str_bia
    THEN
      id_ylv, id_kii, id_ckq, id_hqp, id_kds, id_bmk, id_xbm, id_ekr, id_axn,
      id_ugu
      <--
      id_kjp
        ((((bool(str_gly$0 <= TRUE) , str_irt$0)
             (struct
                (id_alj: (str_qle(str_kkk$0)),
                 id_aff: str_phf,
                 id_asd: FALSE,
                 id_yww: NATURAL1)))
          ,
          NAT)
           'id_akd,
         ({ id_pic, id_gsj, id_ulh, id_ckd, id_fgx, id_uuz, id_tcd, id_gnj,
            id_xmz, id_yvc, id_feg
            |
            str_wkw >=
            rec
              (id_nfi: str_ncq,
               id_xss: str_twa$0,
               id_tzd: str_pah,
               id_fkj: str_lrc,
               id_dqy: str_mgh$0,
               id_qdz: NATURAL1,
               id_lva: NAT,
               id_hkn: str_scn) &
            str_xrn /= str_nfd$0 }
          |->
          struct
            (id_uvg: (left(MININT)),
             id_lqb: str_ghj$0,
             id_jpy: str_soo$0,
             id_rep: str_zxz$0,
             id_wza: str_cij)),
         (bool
            (#
               (id_dni, id_fao, id_xny, id_xqb, id_tyy, id_yld, id_yta,
                id_din, id_lxw, id_kwu).
             (str_ngk : []) &
             NAT : str_hct$0)
            'id_ivy
            (tree(str_nlp(str_uof$0)))))
    ELSIF
      (!(id_all, id_hwv, id_krt, id_qkl, id_zrg, id_yuc, id_ded).
       (INTEGER >= str_nnn) &
       #(id_ixt, id_ywa, id_vbo, id_eah, id_dvy).(str_baz /<<: str_dmc)) <=>
      not(str_ria = str_tbd)
    THEN
      id_vpl, id_tog, id_epg, id_nbb, id_zis, id_uzm, id_oee, id_pds, id_rlb,
      id_ubv :(
        UNION
          (id_orp, id_qyj, id_lnu, id_oou, id_uqu, id_ttm, id_dqj, id_bee,
           id_azu).
        (str_kqr$0 /= 9| str_rjv(INT)) /<<:
        NATURAL(rec(id_euh: str_wcr$0, id_lcz: str_hcw, id_xcy: str_wil)) or
        (str_zlz$0 > str_eaq$0 => str_smk$0 < str_rdu$0)
      )
    ELSIF
      not
        (!(id_uti, id_hnj, id_qgg, id_dpc).
         (bool(str_qaf /<: str_dgd) >= str_xgm) <=>
         TRUE /<: str_qlv)
    THEN
      skip
    ELSIF
      #(id_xvj, id_qza, id_lze, id_rwl, id_pyc).
      (!(id_pme, id_akc).
       (#(id_age, id_gen, id_wwj, id_kmq, id_llm, id_fan).
        (!(id_gtj, id_cyh, id_qly, id_ahf, id_rvh, id_fup, id_hzq, id_tgi).
         (str_djg$0 > MININT))) =>
       (str_ujh < str_ous$0 & str_nnp$0 < {}))
    THEN
      id_thj, id_hkd, id_zvr :(
        not
          (!
             (id_run, id_hzi, id_udu, id_wwa, id_aiz, id_bdm, id_oek, id_xkv,
              id_rxh, id_bjx, id_bjf).
           (#(id_hsc, id_tga, id_ynd, id_fkb, id_vac).
            (%
               (id_tdc, id_jdv, id_nvs, id_itu, id_xxs, id_lso, id_sgk,
                id_gep, id_rpv, id_abw).
             (str_avw >= {}| str_kux) /=
             str_mrw$0
             ,
             rec
               (id_jto: MAXINT,
                id_gmm: str_fji$0,
                id_sae: FALSE,
                id_aqp: INT,
                id_lga: NAT))))
      )
    ELSIF
      rec
        (id_eyp: str_wpo$0,
         id_djc:
         bool
           (#
              (id_toe, id_pdw, id_jpp, id_ing, id_aez, id_lgv, id_vxq,
               id_bdp, id_lqf, id_kif).
            (str_fsn$0 > MININT)),
         id_vbj: str_kbp,
         id_qnv: str_gvv) <=
      bool(#(id_cjd).(str_eev$0 /: str_fov))
    THEN
      id_qdx'id_hwr
      :=
      { id_bbe, id_xcd, id_ecv, id_isd, id_fku, id_jgk, id_phe, id_cyv,
        id_wcc, id_sat
        |
        [NAT1,
         str_qte$0,
         MININT,
         str_ehy,
         str_zqf,
         NAT,
         NAT1,
         PI(id_tjm). (NAT1 = str_yse| str_eza)] <=
        NATURAL }
        (bool(not(str_rux$0 > str_gkz$0)))
    ELSIF
      #
        (id_eje, id_nwq, id_zmz, id_hek, id_bvd, id_ypq, id_int, id_nhr,
         id_tpt, id_hxq, id_fms).
      (not
         (#
            (id_nyv, id_sjy, id_pgw, id_fuw, id_nha, id_sgl, id_iiy, id_gjz,
             id_suu, id_nsq).
          ((str_pnz$0 /= MAXINT & NAT <<: []) or
           (str_nwc$0 /: str_dmn <=> str_tcv /= str_vlo$0))))
    THEN
      id_asx, id_tcb, id_poh, id_eaq, id_rcl, id_qcj, id_ssh, id_zdb, id_toq
      ::
      { id_euk
        |
        #
          (id_mcy, id_paa, id_fao, id_yhd, id_tzj, id_ucm, id_trz, id_wey,
           id_tfo, id_sgo).
        (#(id_bmw).(str_obq < bool(not(str_ycc$0 = str_qdq)))) }
      |->
      str_pla$0
    ELSIF
      !(id_myw, id_fum, id_gyl, id_gok, id_ytt, id_dxu, id_otd).
      (not(str_coe < str_nfs$0) <=>
       (!(id_yjy, id_ujv, id_igs, id_adp).
        (#(id_nhq, id_uok, id_vmr, id_xro, id_gmy, id_hls).
         (str_dnf >= str_vtc)) =>
        #
          (id_ybd, id_sqs, id_bds, id_ref, id_tum, id_kcn, id_rlx, id_vvl,
           id_lzf, id_fkb, id_fcd).
        (NAT /: str_fgr)))
    THEN
      id_iid, id_ohz, id_cww
      <--
      id_zqz
        ({ id_nhn, id_czb, id_chw
           |
           !(id_ygo, id_rap, id_wzx, id_vhm, id_ppd, id_dqk, id_mgm).
           ({{NATURAL1,
              str_xfr,
              INT,
              [],
              (str_fho$0 , str_rxf$0),
              str_jqg,
              []}} <
            PI(id_inv, id_wat, id_cby, id_cog, id_bjl).
            (str_zrf /= FALSE| str_pun)) },
         PI(id_qqp, id_tqh).
         (!(id_hbw).
          (!(id_hhq, id_xdd, id_cvk, id_wuy).
           (not(TRUE < str_zxs$0) <=> str_tvj <: str_wlj$0))|
          str_vor ..
          rec
            (id_dyf: INTEGER,
             id_rix: str_agp$0,
             id_vhe: NAT,
             id_hbt: str_xjm$0)),
         (struct
            (id_omd:
             INTER
               (id_jht, id_sqm, id_xap, id_alr, id_zki, id_ply, id_qwo,
                id_ssa, id_lee).
             (str_dvi <= BOOL| str_fzi$0),
             id_hff:
             (struct(id_pcp: str_uko, id_fcw: str_vbi$0, id_meh: str_jpy$0)
                (str_hwh$0)),
             id_wvk: str_iij$0,
             id_afu: str_jaa$0'id_nwy,
             id_tcg: NATURAL,
             id_scd: (str_wdz(INT)),
             id_pww: {})
            (str_uau$0)),
         (FIN1
            (rec
               (id_qgt: str_zbz,
                id_hkq: [BOOL, NATURAL, str_vus'id_att, {}, str_iwk$0, {}],
                id_xub: str_fhl,
                id_xra: str_qrw,
                id_dsr:
                { id_ghv
                  |
                  not(!(id_qtl, id_jgr, id_rzu).(str_vpa$0 >= str_uxl$0)) }))),
         [struct
            (id_njt: (MININT([str_kgi])),
             id_rgl:
             rec
               (id_unc: str_rua$0,
                id_baf: str_dto,
                id_afo: (str_gpl$0 , str_yzb),
                id_ajq: str_tna$0,
                id_gcs:
                { id_bje, id_vsm, id_zxc, id_dte, id_drk, id_cap, id_ruw,
                  id_biz, id_awo, id_eys, id_bza
                  |
                  str_mqe$0 <= str_plq })),
          BOOL],
         {str_kjg,
          INTER(id_lyq, id_qvw, id_wiy, id_arc, id_ipm, id_ykn, id_mdw).
          (#
             (id_pfx, id_mqv, id_yhn, id_udj, id_xjz, id_cqy, id_reh, id_kov,
              id_tcs, id_boh).
           (str_rdu$0 <= str_hth$0)|
           str_vph$0),
          str_ulj,
          str_led,
          struct
            (id_ctl: NAT,
             id_qvc: TRUE,
             id_bqf: str_cce$0,
             id_fjs: str_njz$0,
             id_qpi: INT,
             id_jyu:
             struct(id_kce: FALSE, id_atz: str_sty, id_cfp: str_jkg$0),
             id_mqd: str_tni),
          INT,
          bool
            (#(id_djz, id_vxw, id_ioh, id_ktl, id_dqf, id_yth).
             (str_zca$0 > {}))},
         [str_ywp,
          (struct
             (id_jlr: INT,
              id_wgt: str_ouc,
              id_brn: str_ikh,
              id_lne: NAT,
              id_quu: str_mab,
              id_lyl: str_jpc$0,
              id_skf: str_vpm$0,
              id_qyv: FALSE)
             (str_szb$0)),
          [str_ofb$0,
           str_nrv$0,
           str_ado,
           rec(id_ana: STRING, id_kav: {NATURAL1, [], str_jad$0, INTEGER})],
          TRUE,
          (FALSE(str_bix$0))],
         bool
           (#
              (id_yaf, id_gfo, id_mjg, id_eyh, id_kcj, id_erg, id_srq,
               id_xeh, id_hfy, id_fgt, id_hkw).
            (str_mbs <
             str_ell >->>
             {str_faw$0, str_lrb$0, {}, str_yde$0, "str_eyg", MAXINT}))
           'id_smn
           'id_yvz,
         [str_mqb,
          str_xqc$0,
          struct
            (id_oeg: str_jsc,
             id_mwy: str_qyn$0,
             id_jiv: [MAXINT, str_vdz$0],
             id_agu: str_trf,
             id_tgf:
             struct
               (id_xzb: str_fuk$0,
                id_vob: str_zhc$0,
                id_qca: NAT,
                id_qft: str_knh,
                id_uys: str_znd$0,
                id_giq: str_dpk),
             id_bea: FALSE,
             id_gno: bool(str_ngj <<: NAT1)),
          (str_qoh$0 , NATURAL)],
         { id_amz, id_iab
           |
           TRUE , struct(id_ymc: {NAT}, id_fst: str_myf) :
           rec
             (id_ivl: str_zgt$0,
              id_irp: INT'id_bbx,
              id_cnp: str_wkh,
              id_bhc: str_yfv) })
    END
  END;

  id_mbg, id_jrs, id_hun, id_him, id_zdo, id_xnx, id_fec, id_cqa
  <--
  id_pcp (id_clb, id_xkx, id_sno, id_dsr, id_lwh, id_bgr, id_txk)
  =
  CHOICE
    id_tny 
      (
        INTER
          (id_ozs, id_dmp, id_bzv, id_bxi, id_ckd, id_lxb, id_fwa, id_nvu,
           id_xan).
        (not
           (str_dfq$0 <<: TRUE <=>
            #(id_ydn, id_anu, id_rql, id_liv, id_ssn, id_hle, id_huo).
            (not(not(str_ouv : str_lun$0))))|
         iterate(str_jdd))
      )
      (
        {str_gcg,
         str_max$0,
         { id_ews, id_mzu, id_dso, id_oqt, id_dks
           |
           !(id_qir, id_rae, id_jge, id_ozj).
           (!
              (id_kbp, id_mnn, id_vcv, id_orr, id_ott, id_irm, id_kpn,
               id_dua, id_dyb, id_anx, id_lrd).
            (#(id_bfo, id_xwg, id_nyf, id_xao).(str_jsp$0 /: str_guo$0))) or
           not(str_tkm$0 /= str_vao$0) }}
      )
      (
        %(id_dwr).
        (str_ole : str_oxb|
         [[],
          (first(iseq1(NAT))),
          str_acf$0,
          (str_nyt$0 ^ str_xpr$0),
          str_gjh$0,
          str_alv$0,
          str_dbi$0]
           (father(str_wxv)))
      )
      (
        [NATURAL,
         str_ski,
         struct
           (id_zyq: str_zfe$0,
            id_kne: {str_scv, str_rxn, NATURAL},
            id_edz: str_fsv,
            id_ima: NAT,
            id_alp:
            struct
              (id_hhf: str_wxe$0,
               id_nzg: NAT,
               id_ssb: (str_vmu$0 |-> (str_uxh$0 >< NAT1)),
               id_gho: str_tej,
               id_wxf: str_ebe),
            id_xsz: NATURAL1),
         rec(id_gsk: str_xjr, id_bmq: str_lha)]
      )
      (
        bool
          (#
             (id_kiw, id_ags, id_hij, id_wle, id_fou, id_hcg, id_qfh, id_axt,
              id_voo).
           (not
              (#
                 (id_omk, id_vjq, id_ohh, id_lir, id_tte, id_rno, id_hdg,
                  id_qyi, id_ulv).
               (not
                  (#(id_xau, id_itt, id_fod).
                   (NATURAL1 <: str_rwi or [] <: NAT1))))))
      )
      (
        { id_irm
          |
          INTER(id_vny, id_jqa, id_bto). (str_ewo$0 : str_rjs$0| []) <
          struct(id_zib: ((str_ymd'id_sfy .. str_gnp$0)(TRUE))) }
      )
      (
        str_yvv
          ([str_bgn$0,
            str_bim$0,
            bool(MININT <<: str_atq & str_rae$0 : str_yaf),
            str_lie$0,
            { id_qgk | str_nxv$0 /: INT <=> INT < INT }])
      )
    
    :=
    (INTER(id_ytq, id_nub, id_icd, id_hdf, id_aar).
     (str_vjd$0 <: {}| str_jaw$0(bool(NATURAL : str_dfa$0))) ;
     INTER(id_ava, id_pmu).
     (!(id_cbj, id_xyk, id_icl, id_fsp, id_vgb).(BOOL > str_xgs)| str_brk$0))
  OR
    PRE
      not
        ((BOOL /= str_uol$0 &
          str_rgw <<: NATURAL1 &
          (not(str_wcu$0 > NATURAL => NATURAL <<: {}) => NAT = NAT)) <=>
         str_jyp /<<: str_lvo)
    THEN
      id_ews, id_hbw, id_lpk, id_wbc, id_vlr, id_fuj, id_itu, id_jmf, id_oeu,
      id_xro
      :=
      INTER
        (id_jqe, id_gkt, id_agz, id_jds, id_oig, id_era, id_bgv, id_aln,
         id_udv).
      (not
         (str_puu$0 >= [] &
          not
            ((NATURAL1 /<<: str_qeo$0 => str_htf = NAT1) or
             str_lxf : str_ste$0))|
       [str_uuj$0, STRING, str_ymn, MAXINT, str_yuh$0, NAT1, str_non$0]) ||
      id_sys 
        (
          [[str_bwo$0, bool(INTEGER > MININT), NATURAL, INT, str_lfi$0],
           struct
             (id_tmm: ((str_cux$0(str_jct$0)) |-> str_ddh$0),
              id_ced:
              INTER
                (id_syh, id_fsg, id_phl, id_rxj, id_wal, id_zkv, id_htp,
                 id_czt).
              (str_igi$0 <<: FALSE| str_ivj$0)),
           FALSE,
           str_fil$0,
           str_nge$0,
           str_bcm$0,
           str_ffa$0,
           BOOL]
        )
        (
          { id_qhx
            |
            not
              (TRUE(NAT) /<:
               TRUE
               |->
               struct
                 (id_bwa: (str_tcx <+ str_ogr$0),
                  id_mkt: "str_roz",
                  id_cgs: str_ubr,
                  id_uzj: str_hyy$0,
                  id_oci: str_qcq,
                  id_pyu: str_fkv$0)) }
        )
        (
          rec
            (id_rii: INT,
             id_lmd: str_qla,
             id_giy:
             %(id_ktw, id_rxz, id_qwq). (str_mnl >= str_qsf| NAT1(str_pfm)),
             id_edo: INTEGER,
             id_jxo:
             rec
               (id_rec: MININT,
                id_ber: str_yck$0,
                id_tdj: BOOL,
                id_fjk: str_bwh,
                id_cmc: str_zll$0,
                id_coc: INTEGER,
                id_kmn: str_kwt),
             id_nks: str_ptz,
             id_rsd: str_ybg$0,
             id_edd: (bool(str_dam /= str_xnz) , STRING))
            'id_uwk
        )
        (
          { id_cgu, id_ipl, id_ceq, id_nwi, id_lpi
            |
            (str_src <: str_qfg <=> str_xmf : NATURAL1) & BOOL = INT }
            (rec
               (id_slz: str_jgz$0,
                id_vos: bool(FALSE <= str_zhq),
                id_bcz: str_abx))
        )
        (
          struct
            (id_cqd: str_npp$0,
             id_fgc: MAXINT,
             id_wwu:
             [str_fds$0,
              (tree(str_ooj)),
              %(id_xrl, id_sbe, id_bhr, id_xgl, id_sdp, id_jmp).
              (str_zkb /= INTEGER| str_amo$0),
              [STRING, str_las, str_ejq$0]],
             id_mwa: str_pel$0,
             id_mry: str_lhg,
             id_eus:
             (str_fil$0
                (struct
                   (id_zdi: BOOL,
                    id_soo: NAT,
                    id_jpx: str_qjv$0,
                    id_kri: str_koy,
                    id_frc: NATURAL,
                    id_zgg: str_nll$0,
                    id_zmv: INT,
                    id_uhl: str_nkd$0))),
             id_guv: str_dxa$0)
        )
        (
          { id_ugb, id_xgg, id_yqo, id_vqm
            |
            #(id_ddy, id_jaj, id_crs, id_xoq, id_ujl).
            (FALSE >= NATURAL <=>
             (NATURAL /: str_foz <=>
              (NATURAL1 = TRUE & not(not(str_odx$0 /: str_ksi$0))))) }
        )
      
      :=
      struct
        (id_bzp:
         ({ id_fsj | MININT = str_xjw$0 }
            (SIGMA(id_rge, id_dib, id_wna, id_url).
             (str_yco$0 /: str_dky| INT))),
         id_dxc: struct(id_bsh: str_kkn$0)'id_qvy,
         id_mov: str_btr$0,
         id_yws: (MAXINT |-> str_uce),
         id_akf: NAT1,
         id_qey: str_uwd$0)
    END
  OR
    id_kxl, id_pqs, id_vft, id_vpf, id_orm, id_phf, id_jyt, id_wyq, id_kda,
    id_bdf, id_kff
    ::
    (left(front(str_oye(STRING))))'id_xjp **
    (TRUE({ id_ufl, id_uey, id_hfx, id_eni | str_xvn$0 /<: str_vek }))
  OR
    ANY
      id_vwn
    WHERE
      !(id_hob, id_qxa, id_lub, id_lxh, id_wmm, id_iiz).
      (#(id_qbw, id_oic, id_rpw, id_mny, id_jsx, id_dth, id_kax, id_lff).
       ((str_oio$0 |-> MAXINT) .. str_mut <<: str_cpf$0)) =>
      (str_pxe$0 : str_fae or str_vru$0 < str_oco)
    THEN
      IF
        ((seq(NATURAL1))(str_htr$0)) +
        INTER(id_bhd, id_hex, id_ile, id_ulp, id_kjn).
        (BOOL /: str_jbo$0 & str_uur <: NAT| str_mjv) :
        str_fnl$0
      THEN
        LET
          id_oju, id_gni, id_rkg, id_wxt, id_cpw, id_wax, id_dhf, id_dez,
          id_wln
        BE
          id_szp
          =
          str_lls
          ,
          PI(id_hlj, id_euq, id_obc).
          (top
             ({ id_rjy, id_azf, id_feg, id_vii, id_dgk, id_bqd, id_rzd,
                id_qhf
                |
                #(id_efs).({} <: NATURAL) }) >
           NAT1|
           {INT, [], str_zib})&
          id_vko
          =
          { id_gvv, id_zsv, id_wxm, id_glb, id_ncn, id_oun, id_dhb, id_meb,
            id_qzn, id_asp, id_vra
            |
            NATURAL <<: str_yho'id_osn }
            (struct
               (id_iwg: bool(str_ltz <= str_skp),
                id_sra: FALSE,
                id_ruf: str_szz$0,
                id_tcx: str_yoq$0,
                id_pod: str_rab,
                id_rxm: str_klq,
                id_vxl: INTEGER))
        IN
          CHOICE
            id_kyb, id_bmo, id_fwb, id_hyy
            ::
            struct
              (id_xru:
               {str_mkq$0, INTEGER, str_icw$0, INT, str_ypf, str_vkt,
                str_dvz$0, str_lsu},
               id_tqq: str_dzg$0,
               id_dtl: bool(str_abe$0 /: str_qhl$0),
               id_ucd: str_vcl,
               id_bqr: str_qbh$0,
               id_zfe: STRING,
               id_gpn:
               [[(str_nuo$0({})),
                 str_sxj,
                 (str_kqy$0 |-> str_vwk),
                 str_sti$0,
                 str_cng],
                MININT])
          OR
            id_otp, id_gck :(
              not
                ({ id_knh, id_ryq, id_kkb, id_wdo, id_dty, id_ozi, id_cqy,
                   id_nwv, id_hhk, id_xgn, id_yfu
                   |
                   str_kuc$0 < str_lvf$0 &
                   #
                     (id_iai, id_sdb, id_nzo, id_gio, id_uev, id_vye, id_knk,
                      id_dqh, id_dcd, id_zgu).
                   (str_ycd$0 /<<: TRUE) } /:
                 { id_zjg, id_nvy
                   |
                   #
                     (id_tvn, id_atx, id_cba, id_tvb, id_wkm, id_ooq, id_ugl,
                      id_esl, id_nwp).
                   (NATURAL : INT) })
            )
          OR
            id_gwx, id_fdg, id_niv, id_rdd, id_sjr, id_evi, id_oei, id_gpx
            ::
            SIGMA
              (id_zmb, id_pab, id_eiu, id_kvj, id_yfm, id_iyv, id_ckq,
               id_sgz, id_kqk, id_hvq, id_soh).
            (not
               (#
                  (id_ooh, id_aft, id_why, id_rwj, id_sqj, id_xmh, id_tec,
                   id_vgb, id_awa, id_cdz).
                (#(id_mek, id_bsx, id_hoz, id_gmt, id_bbj, id_civ).
                 (!(id_ixf).(str_akn$0 /= str_pud$0))))|
             str_luw$0 |-> (subtree(str_ykv)))
          OR
            id_nte, id_trh, id_tml, id_eso, id_cpb, id_afk, id_quc
            <--
            id_pwg
              ({str_yrs$0,
                str_dxa$0,
                bool
                  (#(id_juo, id_qbt, id_pua, id_izz, id_onm, id_bbn, id_sjb).
                   (#
                      (id_wbh, id_xjp, id_kns, id_fem, id_nxr, id_srt,
                       id_het, id_frt, id_cqr, id_byc, id_vjf).
                    (not
                       (!
                          (id_rxq, id_hcq, id_lwa, id_lyq, id_umb, id_phb,
                           id_fbn).
                        (str_cbb <: str_cdd$0)))))
                  'id_ipk},
               {str_wff,
                str_pfz,
                NAT,
                NATURAL1,
                struct
                  (id_qmr: str_lbz,
                   id_yuf: str_nwd$0,
                   id_vsa:
                   (iseq1
                      (SIGMA
                         (id_fbh, id_odz, id_eem, id_buh, id_rum, id_gcf,
                          id_mvo, id_jtb, id_fkh, id_fjs).
                       (!(id_zyh, id_ixj, id_czh).
                        (NATURAL <<: str_cat$0 & str_hua <: NAT)|
                        str_ptj$0)
                         'id_sny)),
                   id_shw: NATURAL1,
                   id_ddi: str_gma),
                TRUE},
               INTER(id_juc, id_euq, id_aet).
               (#(id_orc, id_dww, id_ehc, id_etw, id_djs, id_hnc, id_vsa).
                (#
                   (id_thp, id_six, id_xiy, id_ulw, id_nmw, id_peh, id_uyl,
                    id_tgj, id_wyw, id_wry).
                 (str_axj$0 /= str_lmr$0))|
                bool("str_luq" < [{}'id_fvz, str_xll$0, BOOL])),
               { id_vuo, id_qjb, id_nra, id_foo, id_mpp, id_yll
                 |
                 #(id_yyt, id_ytv, id_ixt, id_vwa).
                 (#(id_tmq, id_yfi, id_thc, id_ubf, id_com, id_ttz, id_eye).
                  (not
                     ([str_gch$0, MININT, MAXINT, str_cie$0] >=
                      %(id_bnl, id_dum, id_mic, id_yto, id_plz, id_cgt).
                      (str_lfo >= str_fcu$0| STRING)))) },
               [str_uhj,
                FALSE,
                str_vvb,
                str_dpj,
                rec
                  (id_wpr: str_jzc$0,
                   id_ikr: str_syk$0,
                   id_our: MININT,
                   id_egq: str_msa,
                   id_bqr: NAT),
                str_mcl,
                str_uko,
                (bool(not(str_hrv$0 <: str_qmc))
                 |->
                 struct
                   (id_ieq:
                    UNION(id_wnh, id_vog). (STRING <<: str_hpc| str_wmd),
                    id_xeg: INTEGER,
                    id_gae: str_hkk$0,
                    id_cff: str_wuc,
                    id_dub: [],
                    id_qlk: NAT1,
                    id_pfx: str_wzo$0,
                    id_ait: str_jeh))],
               [((iseq(INT)) -->
                 {rec
                    (id_bur: str_ica$0,
                     id_irb: [],
                     id_tni: MAXINT,
                     id_khb: NATURAL,
                     id_brj: str_hjt,
                     id_eph: FALSE,
                     id_gtd: str_ybn$0),
                  str_smj,
                  str_wew,
                  STRING,
                  str_jfh}),
                PI(id_wsw). (not(NATURAL : {})| MAXINT),
                str_jow$0])
          OR
            id_iec'id_gqa
            :=
            SIGMA(id_zfc, id_spp).
            (#(id_kom, id_eiy, id_wrh, id_oqr).
             (str_irr$0 < FALSE <=> NAT1 : MININT)|
             str_bew |-> BOOL)
              'id_kuz
              'id_gjm -->
            str_ncw
          END
        END
      ELSIF
        not(not(str_wie /: [])) <=>
        !(id_skj).((str_qkr$0 mod str_nti)~ < str_bbd$0)
      THEN
        id_wkf 
          (
            PI(id_vwr).
            (INT <:
             {str_zox$0, str_zvx, str_zrk, str_jvm, str_qdf, str_wux,
              str_vyl, str_nii} <=>
             str_qji /: str_xfp$0|
             max(struct(id_wra: str_pof$0, id_ova: {INTEGER, str_ylx})))
          )
          (
            father
              (rec
                 (id_nxk:
                  (bool
                     (!(id_muc, id_wlh, id_hri, id_dkh).(NATURAL1 < str_bxx)) |>>
                   bool(MININT /<<: str_tyr$0)),
                  id_kld: (str_aok$0(str_xnr$0))))
          )
          (
            [rec
               (id_kwd: str_dbx,
                id_kgb: str_zlw$0,
                id_hkg: str_uvp,
                id_vwv: NATURAL,
                id_idl: INTEGER),
             str_deb$0,
             str_xrl,
             (FALSE +->> str_tjb$0),
             [],
             MAXINT,
             str_uon$0,
             MAXINT]
              (struct
                 (id_bsb: str_cnq$0,
                  id_mrz: str_mrc$0,
                  id_lnh:
                  struct
                    (id_mkf: NATURAL,
                     id_kai: NATURAL,
                     id_gre: STRING,
                     id_swc: NATURAL1,
                     id_vao: str_rdc),
                  id_tyi: [INT, str_wvv, {}, str_esy$0],
                  id_nbn: INT,
                  id_olk: str_kaa$0))
          )
          (
            SIGMA(id_qhn).
            (bool(str_rqc = str_ojf$0) /=
             rec
               (id_jpu: (BOOL , str_leg$0),
                id_cve: str_wkq,
                id_fjl: NATURAL,
                id_gvh: str_uzq$0,
                id_xci: FALSE,
                id_yos: NAT)|
             ({ id_bcn, id_fzx, id_qhi, id_bcw, id_wbo, id_pcz, id_jol,
                id_dju, id_ckg, id_dgr
                |
                BOOL < TRUE } ;
              str_qrf))
          )
        
        :=
        { id_ftr, id_snp, id_vvu, id_ovy, id_rfa, id_fyd, id_esa, id_kuj
          |
          !(id_hgl).
          (!
             (id_jbt, id_xlo, id_lrp, id_qjv, id_tyd, id_uem, id_xgv, id_vgf,
              id_xqv, id_rfn).
           (not
              (!
                 (id_ndp, id_zxl, id_ccx, id_ses, id_nwe, id_kmy, id_gqh,
                  id_jft, id_ymi).
               (#(id_oco, id_amb, id_jbc, id_eib, id_fem, id_ops, id_qao).
                (!
                   (id_oqb, id_acm, id_zzz, id_xpv, id_zgc, id_emq, id_cgm,
                    id_tst, id_wry, id_ciu).
                 (FALSE : str_fnv)))))) }
      ELSIF
        {} : MININT &
        (#(id_vzg).(not(INTEGER > str_han) <=> not(str_ykk$0 = str_fjv$0)) &
         str_mbq > str_gbb)
      THEN
        id_spd, id_zfk, id_ohs
        ::
        prefix
          ({[str_aot$0, STRING, str_shg$0, STRING, NAT1],
            str_kgt$0,
            str_nnk$0,
            str_jeg$0,
            {TRUE, str_tlj, str_zxt$0, str_mwa, str_caf$0},
            str_pyb,
            [FALSE, str_xth],
            (SIGMA(id_kqm). (TRUE < str_xet$0| NAT1) |-> str_vrh$0)})
      ELSE
        id_xne, id_ngv, id_lyk, id_bcv, id_riq, id_wzw, id_qsa, id_nut,
        id_noy, id_dwj, id_dpz
        :=
        PI(id_yzt, id_ecy).
        (str_qjp$0 <= str_nsc$0|
         (struct
            (id_nsb:
             rec
               (id_van: str_cjb,
                id_rgs: str_ftd$0,
                id_ili: str_skf,
                id_lke: str_lmo,
                id_veo: str_him$0,
                id_blc: [],
                id_dpj: str_kxt$0,
                id_mwx: MAXINT),
             id_qpt: TRUE,
             id_gan: str_lpj$0)
          ,
          INT)
           (str_sxh$0(str_qal$0)))
      END
    END
  END

END 