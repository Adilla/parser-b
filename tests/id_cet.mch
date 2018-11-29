MACHINE
  id_cet( id_buy, id_avs, id_say, id_nre, id_ryy, id_zxh, id_ksl )

CONSTRAINTS
  [str_npp, str_krl$0, str_zvn] /=
  rec
    (id_xgw:
     [((str_sgl$0 , str_mrd)(str_hrz$0)),
      %(id_vsq, id_caf). (str_uru /<<: str_kvp$0| str_ugs)])

SEES
  id_bva, id_tok, id_cxy, id_bad, id_ael, id_rhp, id_cwj

INCLUDES
  id_qjw (
    {(struct
        (id_lfg: { id_nqy | str_zhv$0 < NATURAL1 & str_hfg /<: TRUE },
         id_gpm: NATURAL1,
         id_sez: str_lzv,
         id_huz: str_drx)
        ({ id_kul, id_ekn
           |
           !(id_byi, id_sxi, id_dyx, id_cik, id_ybd, id_xmz, id_syi, id_etp).
           (MININT /: INTEGER) }))},
    { id_hke, id_hpu, id_rzp, id_iip, id_dfm, id_zew, id_wnw, id_rrx
      |
      #(id_fzn).
      (#(id_cvi, id_iaw).
       (!
          (id_bqy, id_tfv, id_zbd, id_trf, id_kfs, id_fxm, id_rhn, id_mfm,
           id_jfx, id_zxb).
        (str_fqn > str_alp)) &
       #(id_xaz, id_fij, id_ynx, id_rxq, id_dbk).
       (str_age$0 <= str_xke or NAT /= str_ctb$0)) }
  ),
  id_rba (
    { id_bsu, id_yar, id_txp, id_fug, id_nrb, id_fdf, id_sfp, id_czz, id_qwh
      |
      ((str_iom <= str_wqe or str_fua$0 > str_deg$0) => TRUE > STRING) =>
      !(id_epk).
      (#(id_fiq, id_zaj, id_xzg, id_epf).
       (#(id_loh, id_mej, id_oyg).(str_sfg$0 <<: {}))) }
  ),
  id_fxg (
    (rec
       (id_ase: [str_wdo, str_tei, str_vpd$0, NAT1, str_szp$0],
        id_npd: NATURAL1)
       'id_wjv
     ,
     ({ id_llg, id_zyj, id_zzg, id_lxe, id_avo, id_pip, id_zcz
        |
        str_lck > str_nei$0 } \|/
      {INT, str_nhx$0}))
  ),
  id_iwj,
  id_fao (
    { id_vbv, id_ifk, id_zvs
      |
      str_wyw(str_ynb) <<:
      fnc
        ([str_bhm$0,
          INT,
          {},
          NAT1,
          str_qqo,
          (front
             (UNION(id_cdu, id_fpt, id_fzv, id_sxq, id_aye, id_qqw).
              (TRUE <: []| {})))]) }
  ),
  id_zza (
    [str_ddq,
     str_xpv,
     { id_emx | str_tvk$0 >= str_hmb$0 },
     (str_bbn$0 , str_vxj),
     (FIN1
        ({ id_qrq, id_yqe, id_osi, id_jxj, id_xbl | not(str_eni >= str_ydy) })),
     str_fqz]
  ),
  id_mhw (
    (str_lum$0
       (iseq1
          (UNION(id_rlo, id_qdl, id_heb).
           (str_fwg <<: str_yim$0|
            [bool
               (str_jpi$0 /<<:
                { id_bxi, id_dgn, id_nee, id_nqt, id_ukf
                  |
                  str_pdv$0 <: NATURAL1 })])))),
    (son
       ((PI(id_rrh, id_kfn, id_oup, id_aey, id_dvv, id_cxj).
         ({} /<: str_fkz| bool(str_bnq >= str_pez$0)'id_snp)
           [[] -->> str_qmy$0])
        |->
        INTEGER))
  )

PROMOTES
  id_aob, id_kpu, id_gbh, id_rxy, id_oyt, id_ofn, id_ltp, id_nfy, id_eku

EXTENDS
  id_gcu (
    [(str_olk$0~),
     str_rrg,
     [(str_xkf$0 |-> str_irf$0),
      ({{ id_jon, id_bhp, id_ldm, id_rww | FALSE = str_rck }, str_gdo$0}
       ,
       str_tvo)],
     str_hti,
     NATURAL],
    ((sizet
        (str_saq
         |->
         { id_dil, id_uya, id_bqt, id_nve, id_crr
           |
           not
             (!(id_bzd, id_sgo, id_qrr, id_sol, id_ebo, id_mxz).
              (str_wyv > MININT & MAXINT < str_xwt$0)) }))
     ,
     str_rbm)
  ),
  id_let,
  id_hdo (
    { id_rci, id_hcn
      |
      str_cyg >
      rec
        (id_mfd: str_hnf$0,
         id_wuy: str_thv$0,
         id_vgf: str_kle,
         id_qhx: str_rco,
         id_alg: str_gsl$0,
         id_tkl: 75) =>
      rec(id_jji: {str_xng, str_gto, BOOL, str_uau$0, str_bff$0}) /<: str_ees },
    (tail
       ({UNION(id_iwh, id_lnm). (str_izb$0 /<: INTEGER| str_tvr),
         "str_kfk",
         STRING,
         ((str_nac$0 +-> str_fks)
          ,
          rec
            (id_dei: str_rlw,
             id_lio: str_qcz$0,
             id_abl: (str_nrv(str_zvg)),
             id_xsk: str_wnw,
             id_nib: BOOL,
             id_rlg: str_dhw,
             id_hld: str_zci$0)),
         NAT1}))
  )

USES
  id_psw, id_vnl, id_bku, id_gcz, id_zbi, id_dyg, id_oyx

SETS
  id_jti;
   id_kcx = { id_pdb, id_zvb, id_iyt, id_dfg, id_ihs } ;
   id_xih = { id_wrr, id_izt, id_sbs, id_axl, id_ojw, id_zct } ;
   id_tju = { id_vkv, id_etf, id_kcd, id_vhl, id_bio, id_rcu } ;
  id_edx;
  id_dzd;
  id_sbn;
  id_cln;
  id_pta

CONSTANTS
  id_zhn, id_iny, id_yki, id_vhc, id_dem, id_sbo, id_dyj, id_ruy, id_nnr,
  id_sub

ABSTRACT_CONSTANTS
  id_kyx, id_eor, id_jge, id_pxv, id_kiw

PROPERTIES
  !(id_zas, id_iwb).
  (not
     (#(id_mcm, id_ngd, id_wkv, id_xej, id_ean, id_uwm).
      (#(id_asp, id_mht, id_opr, id_vne, id_got, id_ynx, id_uke, id_cnc).
       (not
          (!(id_ddv, id_kvw, id_ind, id_eth, id_keg, id_wyb, id_pvt).
           (str_qlp < str_kpx$0))))))

CONCRETE_VARIABLES
  id_vem, id_ffn, id_fgu, id_cif, id_ina, id_onc, id_gcd, id_hbm

VARIABLES
  id_blf, id_gxc, id_jlh, id_erz, id_sji, id_diz

INVARIANT
  !
    (id_ihi, id_wxn, id_kuv, id_yof, id_iop, id_uas, id_ozn, id_dbu, id_hvm,
     id_rls, id_qsk).
  (not
     (!(id_kob, id_nex).
      (not
         (#(id_ngf, id_yxu, id_bhz, id_mro, id_vih, id_afy).
          (MAXINT /<<: str_iki) <=>
          !(id_iqp).(NATURAL /<<: str_vru)))))

ASSERTIONS
  str_mlc$0 :
  (str_iqd +->>
   SIGMA
     (id_zpu, id_jhp, id_bwk, id_voj, id_tzv, id_mel, id_sly, id_kiv, id_ijp).
   (#(id_idv, id_hcy, id_bqr, id_irx, id_tcw, id_obx, id_diq).
    ({} > str_nuo$0)|
    str_qsy))
  ,
  (INTEGER(str_osz$0(INT)));
  str_crg$0(TRUE) :
  (seq1(NAT1))
  ,
  { id_gta, id_uta, id_qdu
    |
    !(id_tll, id_wcz, id_aer, id_vtx, id_ckh, id_xel, id_dva, id_ejm).
    (str_gfv$0 : MAXINT) };
  !(id_qkn, id_nwr, id_fqi, id_mgf).
  (not
     (str_cqg$0 = str_gjj$0 <=>
      (str_kdn > str_nlp$0 <=>
       (BOOL /= str_gec$0 =>
        (str_hsm /<: str_ltp =>
         !(id_cqd, id_kcl, id_qvk, id_byu, id_cgn).(str_mby$0 <= BOOL))))));
  !(id_htt, id_dec, id_wuu).
  (not
     ({str_qjm$0, NAT1, NATURAL1} - MININT /<<:
      [str_mmz$0,
       (str_oqi$0(str_gpo)),
       INT,
       str_xyt$0,
       str_liu,
       str_mxe,
       str_wox]));
  #
    (id_oml, id_jvb, id_icn, id_bwu, id_php, id_kmj, id_fsr, id_rjt, id_gft,
     id_yaw).
  (!(id_jhq, id_aab, id_vuo, id_sjx, id_med).
   ((!(id_sbj, id_gtk, id_ypu, id_iid, id_ada, id_wws, id_aqo, id_rgy).
     (NATURAL1 /<: str_vua$0) <=>
     str_jkv$0 /: FALSE) =>
    not(str_uqt$0 <= TRUE or str_jue >= str_zqe)));
  !(id_lin, id_xch, id_rwp, id_sch).
  (SIGMA(id_zan).
   (#(id_ymb, id_opt, id_ynq, id_qgb, id_rrc, id_awc, id_eey).
    (str_jco$0 /<: str_ngu$0)|
    rel(TRUE'id_unc)) /:
   INTER
     (id_orc, id_dnt, id_pnh, id_dja, id_ezw, id_ebn, id_iri, id_mff, id_pmb).
   (str_hoj$0 <<: {}| str_sdr$0))

INITIALISATION
  LET
    id_pmi, id_eso, id_xsv, id_joq, id_zrq
  BE
    id_ytd
    =
    bool
      (not
         (str_smi /<<: str_jhx$0 or
          (not(str_fuv$0 : str_pww$0) or
           #
             (id_vyr, id_zkk, id_tvl, id_oyl, id_wdg, id_ivr, id_sta, id_sez,
              id_eob, id_bai, id_raq).
           (str_spz <= "str_ess"))))&
    id_jjh
    =
    bool(INT <: BOOL)
      (rec
         (id_yxc: (closure1(str_oim$0)),
          id_lkg: str_bwe,
          id_dad:
          (bool(!(id_ive, id_zqq, id_pyj).(str_pbx /<<: str_kvo$0)) mod
           str_uxv$0)))&
    id_gue
    =
    struct
      (id_djk:
       (struct
          (id_umr: str_sju$0,
           id_mqk: str_utd,
           id_jgf: str_oro$0,
           id_yzm: str_cow$0)
          (MAXINT)),
       id_czq: str_pip,
       id_rqz: str_euu$0,
       id_qli: {})
      ({ id_tgq, id_bxc, id_wrp, id_czy, id_jtz, id_des
         |
         { id_jro, id_qqz, id_nsw, id_yiw, id_wvz, id_bpi, id_lxg, id_nop,
           id_kks
           |
           str_hok >= str_stk$0 } <:
         BOOL })&
    id_eao
    =
    rev
      ([((str_eth$0 / (MAXINT |-> NATURAL)) <<| {str_yhj, str_psf}),
        (str_udd /|\ str_tla)])&
    id_bxs
    =
    (seq
       ((NAT1(str_ncn$0))
        |->
        struct
          (id_hgi:
           INTER(id_ioq, id_rbb, id_hly, id_xyz, id_vdp, id_zjt).
           (90 <<: NATURAL1| STRING),
           id_ccu: INTEGER,
           id_qfm: str_mcr)))
      (NATURAL1 |>> NATURAL1)&
    id_taj
    =
    SIGMA
      (id_bng, id_maz, id_fgy, id_dzv, id_jxf, id_mix, id_gre, id_ggm,
       id_heh, id_eeo, id_gof).
    (str_wka$0 = str_dai =>
     !
       (id_ojp, id_nka, id_dhy, id_nco, id_sdh, id_yid, id_djw, id_oxh,
        id_obe, id_xql, id_flh).
     (str_gpr /= str_gin$0 => NATURAL1 > NAT)|
     BOOL <- str_spo$0)
      'id_som |>>
    str_lda&
    id_dnb
    =
    (str_rwh$0 |-> (rec(id_xsk: str_ezj) <+ str_ojf$0))
      (rec
         (id_qbw: INT,
          id_gey:
          struct
            (id_hks: str_lqf,
             id_kfn: struct(id_grn: str_zel, id_uki: str_myh))))&
    id_avj
    =
    { id_zwy, id_wed, id_dpj
      |
      !(id_wdp, id_rqb, id_wqs).
      (#(id_xtu, id_aao).
       (!
          (id_vrk, id_aaj, id_ovd, id_bkh, id_mds, id_dkf, id_zim, id_iho,
           id_lkk, id_tfn, id_cka).
        (!
           (id_tzf, id_web, id_tko, id_bzw, id_tpg, id_jmm, id_vrq, id_axf,
            id_bcc).
         (#(id_emu, id_gaw, id_xsx, id_wzt, id_uwq, id_ecy, id_wwo).
          (not(NAT1 > str_dbj$0)))))) }&
    id_nyp
    =
    {FALSE,
     str_sdy,
     NATURAL,
     str_zzl,
     struct
       (id_nhn: str_gzl,
        id_xsh: str_gve$0,
        id_xet: str_xds$0,
        id_nzc:
        { id_jzq, id_rll
          |
          #(id_kou, id_rab, id_diy, id_kij, id_mxv, id_nqt, id_vgd).
          ("str_sje" : str_lqa) },
        id_ohc: str_lks,
        id_oap:
        struct
          (id_nxo: (643 |-> NATURAL1),
           id_tny: "str_hon",
           id_kqe: NAT,
           id_zpa: NAT)),
     { id_wlr, id_zyc, id_tiy, id_nqb | STRING <= str_ate }}&
    id_pdm
    =
    [BOOL, str_msy, BOOL]
    ,
    ((str_wci$0
      ,
      { id_jes, id_fhf, id_bgf, id_pcb, id_wym, id_vum, id_hwa, id_tnx,
        id_bne, id_swj, id_zyo
        |
        bool(str_rkh /<<: str_uhe$0) : NAT })
     ,
     str_quv$0)
  IN
    LET
      id_qio, id_xun, id_tbx, id_zna, id_qpo
    BE
      id_dmd
      =
      son
        ([str_jtx$0'id_ppf, ({} |>> {}'id_bpd), str_afc]
           (str_jzn$0 , str_mug$0))&
      id_nmf
      =
      struct
        (id_ywd: INT,
         id_pph: str_zgi,
         id_qcw: MININT,
         id_qos:
         INTER(id_rnh, id_rda, id_gnq, id_wby, id_tui, id_vwr, id_oog).
         (!(id_anb, id_gvv, id_zjy, id_exq, id_hzw).(STRING < str_oot$0)|
          [str_vlx,
           str_udx,
           str_xjy,
           str_vjp,
           {MAXINT, str_sje, str_phu$0, FALSE},
           MAXINT,
           (str_agk ** str_ivz),
           NATURAL1]),
         id_yzd: str_qfh,
         id_hgx: TRUE,
         id_pxg: str_hvx)&
      id_bsz
      =
      ([str_ydk$0, TRUE, MININT, str_lmc, {}, {}, NATURAL1] / NAT)
      ,
      bool
        (!(id_lcg, id_qlr, id_zhn, id_fky, id_dqe).
         (!(id_ytv).(!(id_ubt, id_oan).(str_txv >= STRING))))&
      id_akj
      =
      bool
        (#(id_qdf, id_wgh).(not(str_yph > str_uqf$0)) =>
         #(id_wwc, id_frz, id_ott, id_whn).
         (#(id_ebc, id_kwa, id_mat).(!(id_zxd, id_csn).(str_nvz = str_szu))))&
      id_pgw
      =
      (str_bct$0
         (rec
            (id_rlo: STRING,
             id_rqg: str_pvu,
             id_tdv: str_yaj,
             id_rmj: struct(id_hqu: BOOL, id_mnv: (FALSE , str_iza)),
             id_rhf: str_weq,
             id_mml: [NATURAL, [str_xdg, str_fqv, str_ssc, str_yha, []]]))) mod
      str_cqs$0
    IN
      WHILE
        #(id_uqy, id_skn, id_qqn, id_jlf, id_rvy).
        (#(id_ndf, id_pll, id_oxo, id_mxg, id_osf, id_npj).
         (not
            (struct
               (id_hcz: MININT,
                id_tvx: str_oax$0,
                id_caw: str_xuu$0,
                id_lbh: str_wlg$0) <
             []
             |->
             INTER(id_ljp, id_dzi, id_mpp, id_atd, id_vrm).
             (str_yrg >= str_iyr$0| str_uww$0))))
      DO
        IF
          !(id_dss, id_xfn, id_vcf, id_ics, id_vnn, id_cyd, id_zeh).
          (#(id_ypf, id_cax, id_opf, id_hwt).
           (not
              (#(id_qjw, id_isi, id_hko, id_fjw).
               (#(id_fta, id_ozn).
                (#
                   (id_cst, id_gsd, id_jmo, id_vvi, id_nqi, id_lke, id_mgh,
                    id_qpf, id_oum, id_mkn).
                 (MAXINT /: MAXINT))))))
        THEN
          WHILE
            [bool({} <= str_wck$0 or str_wtn$0 = NAT),
             str_jan$0,
             str_kyz,
             (BOOL , str_fqz),
             str_nwu,
             [],
             str_ysv,
             { id_uyr, id_cbs, id_wtz, id_apc, id_lkb, id_fym, id_jka, id_ore
               |
               str_myk /= str_zjp$0 }] <=
            str_opx
          DO
            id_uix, id_ajs, id_cvf, id_dsi, id_yqo, id_hpq, id_hfc, id_eib :(
              !(id_nyt, id_lqa, id_llh, id_svg, id_kjr).
              (!
                 (id_yge, id_dta, id_ykg, id_vwi, id_qzm, id_yqg, id_yji,
                  id_fqz, id_xnx, id_cfj).
               (!(id_mml, id_frs, id_fmu, id_vet, id_jpj).
                ([] /<: str_hpn$0 <=> not(str_sph$0 : str_zkk))) or
               (str_wwy$0 <: [] => [] /= INTEGER))
            )
          INVARIANT
            not
              ([str_qdi$0, MININT, str_lkp, str_rse, MININT, str_vrc$0] <=
               UNION(id_hfl, id_qkh, id_ouf).
               (str_ltk <: NATURAL1|
                { id_njs, id_zmp, id_mvs, id_lzw, id_zwv, id_hhd
                  |
                  !(id_woj).(#(id_pyz, id_ngv, id_xrp).(str_xuj$0 /<<: NAT)) }))
          VARIANT
            struct
              (id_vvz: str_gxz$0,
               id_bkv: str_uxc,
               id_usb: MAXINT,
               id_cii:
               (POW1
                  ({ id_dah, id_ltc, id_qcv, id_ozp, id_nfl, id_dns, id_rqs,
                     id_vlw, id_onf, id_mqt
                     |
                     !(id_vdj, id_bch, id_jvi, id_duv).([] <<: str_zgm) })),
               id_ele: bool(#(id_anz).(str_nwu <<: str_vpb)),
               id_gja:
               PI
                 (id_npk, id_rch, id_jry, id_qng, id_siz, id_pds, id_nrj,
                  id_qwy, id_jdr, id_opi, id_xzx).
               (MAXINT /<: str_swl$0| 83))
          END ||
          id_kiq, id_igj, id_bga, id_hqc, id_bui, id_man :(
            #
              (id_flv, id_wso, id_mvh, id_ona, id_nyn, id_ycx, id_qqu,
               id_pmu, id_lpu, id_trq, id_fio).
            (#(id_xhy, id_hbh).
             (str_tix$0 >
              {struct
                 (id_exh: str_ncn,
                  id_tig: NATURAL,
                  id_fnf: str_lza$0,
                  id_xjg: str_aoc,
                  id_esk: str_blo,
                  id_snp: BOOL),
               (sizet(str_uae$0))}
                (str_esl$0)))
          )
        ELSIF
          #(id_yfk).
          (!(id_hxc, id_kms, id_ody, id_lao, id_suo, id_mij, id_sad, id_dbd).
           (#(id_oiv, id_kjs, id_rij, id_gyo, id_rzk, id_vbg, id_vgl, id_cej).
            (!
               (id_rms, id_jvy, id_bee, id_bfw, id_ydb, id_nwk, id_vtb,
                id_ola, id_iiu).
             (str_eao$0 <: str_zwz) &
             #(id_xah, id_xmj).
             (!
                (id_qfo, id_qqg, id_bdg, id_dxn, id_tvi, id_thr, id_ntp,
                 id_mcy, id_rkd, id_ctv, id_way).
              (str_gnj$0 /: str_dcz$0)))))
        THEN
          id_lbc, id_ttt, id_qlk, id_pvm, id_jmc, id_dlk, id_vpp
          :=
          [(NATURAL1 >->
            { id_yrq, id_bhj, id_nar, id_jzd, id_inu, id_msj, id_ihi, id_srd,
              id_doh, id_oxk
              |
              str_zsj <= str_dlw or
              struct(id_uqz: str_dcv$0, id_tfm: (INTEGER |-> str_ikr$0)) <<:
              str_soy })]
        ELSE
          id_bre, id_iiy, id_sxw, id_qrd, id_sks, id_cxb :(
            not
              (not
                 (struct
                    (id_zix: NAT1, id_cuv: { id_fuz | not(not(2 = str_rpp)) }) /:
                  str_hqw))
          )
        END ||
        skip
      INVARIANT
        not
          (!
             (id_alu, id_azo, id_dlr, id_iej, id_eok, id_kwt, id_zsq, id_tyt,
              id_bou, id_tgb).
           (!
              (id_ere, id_edu, id_cai, id_hap, id_crm, id_ctx, id_rhy,
               id_eof, id_uqr).
            (str_lso$0 , str_rca'id_wok <<: [str_zny$0])))
      VARIANT
        struct
          (id_hxa:
           ((struct
               (id_eju: str_jne,
                id_rwy: str_dct,
                id_gzm:
                struct
                  (id_bak: 3,
                   id_wey: str_txy,
                   id_owx: INTEGER,
                   id_nbl: "str_rct",
                   id_ycx: NAT1,
                   id_pfu: STRING),
                id_nua: STRING) -
             (str_cap$0({})))
              ([])),
           id_pjj: str_kqx$0,
           id_dhq: str_oni,
           id_tka: str_jzh,
           id_bjk: STRING)
        |->
        TRUE
      END
    END
  END

OPERATIONS
  id_fek, id_dtq, id_wsw, id_dlh
  <--
  id_wmr (id_nas, id_wsb, id_rsb, id_fiv, id_lqi)
  =
  ANY
    id_nab, id_dka, id_ztr, id_vpb
  WHERE
    #(id_icy, id_qlq, id_tul, id_aua, id_hdz, id_nir, id_tfh, id_tsd, id_jpi).
    (!(id_jul, id_oog, id_hbd).
     (str_jjs$0 <: str_phk$0 &
      (!
         (id_kcb, id_ggq, id_qen, id_xrk, id_efo, id_xxo, id_ltf, id_svj,
          id_nwd, id_vkt).
       (#(id_ygo).(str_dkt > str_hfa)) =>
       not(NATURAL1 <= str_cnw))))
  THEN
    IF
      rec
        (id_osf: MAXINT,
         id_wzg:
         struct
           (id_jrt: str_ldn,
            id_vnv: NAT1,
            id_iza: rec(id_qem: str_qkl$0, id_kqo: str_bpw),
            id_ujw: MININT,
            id_knf: str_bdd,
            id_nps: [],
            id_sok: str_vlw)) <<:
      bool(infix(str_rcv) >= INT)
    THEN
      LET
        id_fvy
      BE
        id_gkh
        =
        { id_knm, id_olh, id_sxa, id_aoq, id_rwv, id_bij, id_lat, id_ypk,
          id_nuz, id_sae, id_ihs
          |
          (str_ieq$0 : str_nig <=> STRING /: str_cfh) or
          not
            (!
               (id_hsi, id_afe, id_pye, id_vnx, id_byl, id_xgx, id_wwr, id_imh).
             (MININT /= NATURAL) or
             str_dph$0 /= INTEGER) }&
        id_qzj
        =
        { id_lac, id_wdh, id_tjj, id_vcy, id_ipu, id_ujp, id_qtd, id_jku,
          id_eie, id_gvd
          |
          {str_nyd$0} <:
          %
            (id_txn, id_wrr, id_amj, id_gte, id_hgo, id_twq, id_cun, id_kul,
             id_cmj, id_xdy).
          (not
             (!
                (id_fwi, id_ekq, id_rqr, id_gvq, id_afa, id_spm, id_yhj,
                 id_tvh, id_ulw).
              (#(id_bor, id_jco, id_wig, id_pii, id_zxh).(str_ier >= str_luu)))|
           str_uej) }&
        id_uvt
        =
        {{},
         str_mrf,
         str_zwx$0,
         ({ id_qvg, id_yen, id_pnn, id_plq, id_hjl, id_xtx, id_ukz, id_igg,
            id_rte, id_afk, id_fnm
            |
            BOOL : NAT1 => str_bwn$0 /<: str_ikr }
            'id_zep
            (bool(MAXINT /: str_ppk)))} +
        str_atl&
        id_rax
        =
        rec
          (id_qwb: BOOL,
           id_sbp: str_ppa$0,
           id_zep: [],
           id_pgw: str_zeq,
           id_jrc: str_nmz$0,
           id_uzl:
           (sizet
              (struct
                 (id_wfw: str_fqf,
                  id_nmg: str_lfj$0,
                  id_enw: str_fde,
                  id_dqd: str_qin,
                  id_ono: str_boy,
                  id_rki: (INTEGER , str_ywx$0),
                  id_xvk: str_dlt))),
           id_jrg: {rec(id_sly: NAT1), []}'id_ouy,
           id_lmg: MININT)&
        id_uns
        =
        { id_rrc, id_wwf, id_aas, id_cfm
          |
          !(id_uvm, id_dbu, id_wkx, id_nxk, id_tmg, id_jyx, id_gnb).
          (rec
             (id_fxr: str_nnp,
              id_zyw: str_pkt,
              id_qzm:
              PI
                (id_gnx, id_ysh, id_tkg, id_pco, id_jyo, id_gec, id_kdh,
                 id_pnf, id_uhp, id_dia, id_mcn).
              (str_dnr$0 : str_jjn$0| struct(id_lyc: str_nkm)),
              id_hlr: str_dyi,
              id_abr: FALSE,
              id_nkg: str_htq) =
           str_nbx$0) <=>
          BOOL > str_vos$0 }&
        id_uun
        =
        bool
          (not
             (struct
                (id_lhb: (FIN1(str_suj$0)),
                 id_uom: str_ine,
                 id_gbm: str_xxa$0,
                 id_uhd: str_vzw)
                (str_mtr$0) /<:
              struct
                (id_gcj: str_ppv$0,
                 id_vgb: str_qgm$0,
                 id_gda: str_kxn$0,
                 id_kvl: str_fmd$0,
                 id_rlg: NATURAL1)))&
        id_srv
        =
        {(str_uzc(INT'id_vla)),
         PI
           (id_qkh, id_fpo, id_gdf, id_wpj, id_vos, id_sau, id_tgp, id_dlr,
            id_jxm).
         (!(id_ynq, id_umf, id_zge, id_nvh, id_zji).
          (!(id_dxg, id_gyq).(str_esv /: MAXINT) => str_gcq /= MAXINT)|
          [])}&
        id_xte
        =
        struct
          (id_vmt:
           [str_ybn$0,
            INTER
              (id_sni, id_yiz, id_hus, id_sap, id_yiz, id_kpn, id_rxj,
               id_evu, id_kkf, id_wbz).
            (STRING /<: str_gee| str_kdk),
            {TRUE, TRUE, str_tky$0, str_wba, str_sni, str_zjn, str_dou$0}],
           id_tyy: (bool(STRING <= str_wzk$0)~),
           id_wvt: str_quw$0,
           id_rwg: str_ueh$0,
           id_rjj: NATURAL)
          'id_urq&
        id_xll
        =
        PI
          (id_vjc, id_exi, id_ftd, id_iwl, id_dch, id_zpq, id_izo, id_ysu,
           id_fpx, id_oia).
        ((str_duy$0 = NATURAL <=> str_mpu$0 = str_wuv) & str_fsa /= str_kzf|
         []) \|/
        PI
          (id_zew, id_qea, id_oek, id_xpm, id_fkl, id_sar, id_xrp, id_bqn,
           id_lks, id_jej).
        (#
           (id_pyj, id_cre, id_pra, id_wxs, id_muz, id_icy, id_kdd, id_tkb,
            id_axh, id_jqm).
         (FALSE = NATURAL1)|
         UNION(id_wbw, id_bvf, id_jxa, id_dve, id_ogc).
         (INTEGER >= str_htm| str_jkf))
      IN
        WHILE
          !(id_rhx, id_zww, id_epg, id_ryh, id_wgh, id_wre, id_cqd, id_jlo).
          (min
             ({str_wad, [], str_xgz$0, str_imz$0, str_fsi$0, str_ena, str_bun}) <:
           SIGMA
             (id_vhk, id_ymq, id_szl, id_rte, id_ipt, id_bkx, id_kcy, id_izf,
              id_aef).
           (not(str_zpq = str_lze$0)| str_oxi$0) <=>
           str_njh > STRING)
        DO
          LET
            id_jwd, id_jgf, id_yfy, id_xsm, id_gkr, id_pbm, id_lps, id_wwx,
            id_fyr, id_dkh, id_kbs
          BE
            id_wki
            =
            { id_izy, id_syh, id_bcr, id_moo, id_rvt, id_iib, id_jav, id_qem,
              id_hep, id_pvz
              |
              not
                (not
                   (not(MAXINT <= str_zhz$0 <=> NATURAL'id_fio /: str_nso$0))) }&
            id_azd
            =
            ([struct(id_clo: INT, id_ypt: {}, id_fuu: str_xfa$0),
              NATURAL1,
              str_zif$0,
              FALSE,
              str_mku$0,
              str_fbm$0,
              str_nbi$0,
              str_wff$0] ->
             str_yyn$0)
              (bool
                 (!
                    (id_fuy, id_cio, id_bjl, id_jfu, id_xfd, id_qnw, id_jqn,
                     id_bxy, id_ghd, id_gnc).
                  (!(id_ckk, id_yjh, id_ptn, id_ftg, id_jgm).(STRING >= BOOL))))&
            id_iax
            =
            rev
              (struct
                 (id_qzg:
                  %(id_bur, id_owk, id_ijl, id_wan, id_jqj).
                  (NATURAL <<: str_zwq &
                   str_imy /=
                   struct
                     (id_gtn: NAT,
                      id_qto: str_qny,
                      id_fqh: str_fsr,
                      id_pxa: STRING)|
                   NATURAL),
                  id_umv: str_roo,
                  id_zpu: str_sse,
                  id_ahh:
                  { id_wxq, id_faf, id_btu, id_xpl, id_gwr, id_pts, id_uki
                    |
                    str_ezl <= NAT }))&
            id_hrq
            =
            conc
              ({ id_pix, id_rmi, id_kgo, id_eim, id_krj
                 |
                 struct(id_zln: str_tub$0, id_hia: str_agl) <=
                 { id_axg, id_shn, id_nvb
                   |
                   str_llh$0 <: PI(id_yng). (str_sjo$0 <<: str_uhd| str_ntp) } })&
            id_uqj
            =
            {str_rwu,
             (top
                (bin
                   (struct
                      (id_slq: MAXINT,
                       id_nfs: str_nen$0,
                       id_pma: NAT,
                       id_gqp: [],
                       id_ykw: str_iae,
                       id_ujb: NATURAL,
                       id_fat: {}))))
               'id_xrh,
             str_ntz$0,
             INTEGER,
             str_oeu,
             bool(TRUE /<: str_pyl),
             str_ktd$0}
              (str_ray)&
            id_dfy
            =
            card
              (struct
                 (id_ejt: str_wiy,
                  id_kwk: str_jde,
                  id_zlh:
                  (min
                     (struct
                        (id_wse: (bool(MAXINT < str_lyh$0) , str_zku'id_lwy))))))
          IN
            id_kjq 
              (
                [{ id_efp, id_hqw, id_eej, id_pkg, id_nqc
                   |
                   !
                     (id_npa, id_itq, id_pju, id_wwz, id_who, id_sbm, id_ngp,
                      id_cju, id_zjv, id_qkk).
                   (not(not(542 : str_fif$0))) },
                 INTEGER,
                 struct(id_aau: str_zti),
                 struct(id_oyn: str_dhl$0)]
              )
              (
                { id_tzp, id_rrg, id_hnv | not(str_zus >= str_vdc$0) }
                  (son
                     (rec
                        (id_rto: str_udm$0,
                         id_ofm: str_prr,
                         id_zth: (str_vtk , (str_qyr |-> str_vou)))))
              )
              (
                { id_xgi, id_fjw
                  |
                  str_xld$0
                    [UNION(id_ily, id_wdl, id_dkm).
                     (str_lce$0 /<<: str_ekp$0| NAT)] =
                  NAT1'id_dgm'id_fki } /
                str_mhv
              )
              (
                {{},
                 str_vvg$0,
                 MAXINT,
                 str_llt$0,
                 str_sug$0,
                 ({ id_req, id_wmi, id_spv, id_vwj, id_mlr, id_wxg, id_hqe,
                    id_jmq
                    |
                    str_gfl$0 <<:
                    {str_nxk,
                     struct
                       (id_xbf: NATURAL,
                        id_zxf: FALSE,
                        id_smv: NAT,
                        id_sqy: MININT,
                        id_lfx: str_uxf,
                        id_xse: FALSE),
                     str_eee,
                     NAT,
                     4069,
                     5,
                     str_fiy$0} }
                  ,
                  (left(str_yul))),
                 str_yjo$0}
              )
              (
                [(id(str_hmk$0)),
                 str_tdm$0,
                 { id_pdv
                   |
                   str_ohb <= NAT <=>
                   (({} < str_uaf => {} /<: str_nis$0) or str_toj$0 <= INT) },
                 NAT1]
              )
              (
                rank
                  ({ id_atq | MAXINT /: str_jes }
                     (bool
                        (not(NAT >= str_wjt or str_vdb$0 <: str_vrj) &
                         str_zmj$0 /<<: NATURAL)))
              )
              (
                bool
                  (str_hxv$0 /<<: STRING or
                   (str_lqn /<: str_ogw or
                    str_bya$0 >= str_gco$0 or
                    str_nwb <<: str_kuj$0))
                |->
                struct
                  (id_fqa: NAT1,
                   id_fbk: str_arn,
                   id_oaa: ((str_gnw$0 || str_ffu$0)))
              )
              (
                { id_mqg, id_eia, id_hzp
                  |
                  { id_juj, id_qzj, id_osr, id_ivo
                    |
                    str_zrp$0'id_jip /= father(str_ywm$0) } <=
                  struct(id_azb: str_pwy$0) }
              )
              (
                (TRUE
                 |->
                 ({str_lrj,
                   str_qrn$0,
                   NAT,
                   str_xyl$0'id_ooj,
                   str_cno,
                   str_fzg$0,
                   str_rcg$0}
                    (str_bwj)))
                  'id_vyj /
                { id_tho, id_oqw, id_omi, id_wjm, id_zjm | str_kpk$0 : TRUE }
              )
              (
                {str_bqb,
                 str_dys,
                 INT,
                 (rec
                    (id_gef: FALSE,
                     id_zns: bool(str_mcy$0 /: str_nyy),
                     id_sev: []) <-
                  ({{}, str_hlj$0, str_hga$0, str_xgt} , str_unq$0)),
                 MININT,
                 NATURAL1'id_nua,
                 STRING}
              )
              (
                bool
                  (#
                     (id_snm, id_rgs, id_uac, id_cvy, id_ont, id_sxl, id_jxm,
                      id_qqn, id_ukg, id_wre).
                   (%
                      (id_jym, id_tzm, id_hal, id_coy, id_bzr, id_afi,
                       id_ubs, id_wrh, id_ifx, id_juj).
                    (#
                       (id_dwm, id_xtw, id_udw, id_hiz, id_kua, id_dis,
                        id_mdt, id_ops, id_oov, id_frg).
                     (str_tvu < str_pqu$0)|
                     succ(top(str_nxc$0))) /=
                    str_zmw))
              )
            
            :=
            (min
               (struct
                  (id_udf: (str_gqo --> str_xvg'id_gcq),
                   id_uvb:
                   PI(id_rsa, id_mlt, id_xeb, id_oeu).
                   (NATURAL1 > MININT| str_kga$0),
                   id_scu: INT,
                   id_krf: str_jco,
                   id_ajn: str_kyn$0,
                   id_chl:
                   struct
                     (id_qbn: NATURAL,
                      id_mac: STRING,
                      id_aui: NATURAL1,
                      id_kiz: str_uhc))))
              'id_qhi
          END ;
          ANY
            id_gii, id_ldj, id_csv, id_vcs, id_byp, id_djy, id_zdj, id_say,
            id_ogn, id_ntk
          WHERE
            not
              (not
                 (rec
                    (id_kuf: str_cnl,
                     id_buh: str_lfj$0,
                     id_dfa: str_jgp$0,
                     id_moe: str_anp,
                     id_ela: MININT,
                     id_qzk: str_mom$0,
                     id_nyb: str_nja$0,
                     id_apu: str_jif$0) /=
                  { id_rrv | #(id_vgq).(not(str_dwl$0 = str_qbr$0)) }))
          THEN
            id_zek 
              (
                (INTEGER |-> str_lhs$0)
                |->
                { id_eff
                  |
                  str_abp /<: FALSE &
                  ((str_mgi > str_kth$0 & str_qwq$0 > str_een$0) <=>
                   (str_ahp /<: str_ojk$0 & str_otp$0 >= TRUE)) }
              )
              (
                INTER(id_vzz, id_wpl, id_brr, id_lzs, id_ihw, id_jdb).
                (str_ibi <= STRING|
                 INTER
                   (id_oth, id_mxy, id_rgw, id_eba, id_jyv, id_bab, id_ten,
                    id_ico, id_ded).
                 (!
                    (id_gqf, id_gee, id_odh, id_tac, id_rth, id_pun, id_oxw,
                     id_ogr, id_ydm, id_mmj, id_pxh).
                  (#(id_gzs, id_btq, id_rzm, id_gpc, id_wch, id_lcv, id_otc).
                   (str_fvk$0 /= {}))|
                  [str_poo$0,
                   str_lwp,
                   NAT,
                   [],
                   struct
                     (id_cgc: str_khn$0,
                      id_sqt: bool([] >= str_bfc),
                      id_aiz: 1),
                   str_zxr$0,
                   str_wke,
                   NAT1]))
              )
              (
                {MININT,
                 PI
                   (id_vry, id_xzm, id_hmx, id_ggk, id_udz, id_osu, id_opx,
                    id_jlx, id_fws, id_xhr, id_ixp).
                 (NATURAL >= {}|
                  rec
                    (id_gpn: str_jkc$0,
                     id_nig: str_yyi$0,
                     id_zxb: str_ueo,
                     id_ore: INTEGER,
                     id_ngs: str_quc$0,
                     id_cya: str_wyv,
                     id_kby: (str_qoz --> 22),
                     id_cfl: BOOL)),
                 str_onp$0,
                 struct(id_kcn: str_dzl$0, id_ody: str_whh),
                 [str_ala, MAXINT, str_slz, str_szz$0, str_dyv, str_jzu$0],
                 str_mnq,
                 UNION(id_gql). (str_ypb$0 < str_mbk$0| str_bej),
                 NATURAL}
              )
              (
                {str_btb,
                 str_srw,
                 str_qmp$0,
                 str_nyl$0,
                 str_rhu,
                 str_kvw,
                 (iterate
                    ((right(str_rvz$0)) /|\
                     (rec
                        (id_jcc: str_uvp,
                         id_wiz: str_lfe,
                         id_iiy: str_kaf,
                         id_xva: str_brn$0)
                        (str_zwk$0'id_dub))))}
              )
              (
                {{ id_woz, id_ywr
                   |
                   not(!(id_lof).(not(str_wjm > str_atb$0))) <=>
                   NAT > str_fii$0 },
                 MAXINT,
                 str_nbe,
                 PI(id_dxl, id_ahg, id_pgd, id_vks, id_wsd, id_kmy).
                 (MAXINT <= str_iqg| str_ipx$0),
                 MAXINT,
                 str_xqi$0}
              )
              (
                { id_nxz, id_vgx, id_hwy, id_vch, id_nmq
                  |
                  !
                    (id_knj, id_gdj, id_hzq, id_kcc, id_qhw, id_hfl, id_cys,
                     id_jaw).
                  (!(id_ite, id_idy).
                   (not
                      (!(id_tum, id_ccc, id_zzd, id_bgc).
                       (not(str_snc <= str_btv$0) & NATURAL = MAXINT)))) }
              )
            
            :=
            {["str_nqo", NAT, str_som, str_zoo$0],
             struct
               (id_wlw: str_klj,
                id_phi: bool(not(str_eut$0 <: str_obw$0)),
                id_vzo: str_vhm$0,
                id_ngi: str_hls),
             (MININT <-> str_wab$0),
             rec
               (id_vvo: str_vmr,
                id_hnh: str_wwb$0,
                id_phe: str_rfv,
                id_esd: str_cze),
             str_szo,
             str_crz}
          END
        INVARIANT
          rec
            (id_tgl: {},
             id_tre: str_zno,
             id_gfj: str_vsn$0,
             id_gmp: str_rrh,
             id_jpa: INTEGER,
             id_frv: (str_uqx$0 |-> str_iei$0),
             id_ddw: str_lrf$0,
             id_xyu: (INT , str_oeu)) <<:
          [INTEGER, str_ccm, (str_frw$0 , str_ghn), (str_ghl$0(str_udq))]
        VARIANT
          STRING
            (bool
               (str_bun$0(str_kxd) >
                btree
                  (rec
                     (id_fvq: bool(str_pej > str_kwp$0),
                      id_rst: str_ynf,
                      id_vxo: str_zxj$0,
                      id_brk: str_uwl$0,
                      id_lqs: NAT,
                      id_hrz: str_zsn$0,
                      id_ryj: str_nyf$0))))
        END
      END
    END
  END;

  id_ygd, id_yiu
  <--
  id_fhx
    (id_qlk, id_wsm, id_nso, id_zyc, id_fxm, id_jwl, id_aoq, id_laj, id_gsc)
  =
  VAR
    id_twe, id_opt, id_fns, id_oyc, id_goj, id_wid
  IN
    ANY
      id_ddt, id_rhv, id_mzx, id_mon, id_dna, id_sxr
    WHERE
      not
        (not
           (!
              (id_ysl, id_tsm, id_xpf, id_ooo, id_whj, id_tbt, id_cys,
               id_rmp, id_bai, id_azy).
            (not(BOOL , (bin(str_juu)) : str_ijp$0))))
    THEN
      LET
        id_zog, id_joq, id_inm, id_jfh, id_hgg, id_fra, id_wob
      BE
        id_ukl
        =
        bool
          (#(id_ikz, id_bmy, id_rjd, id_ipu, id_qfq, id_zpc, id_yon, id_szm).
           (not
              (not
                 (str_xku <=
                  { id_lbr
                    |
                    #(id_jra, id_rha, id_bqf, id_xzm, id_bhs, id_wti, id_cpa).
                    (str_vrp /<<: BOOL) }))))&
        id_hnr
        =
        rec
          (id_nmu:
           rec(id_qxd: MININT, id_ukb: INT, id_eht: str_qbj, id_mhh: BOOL),
           id_eqt: {str_olm$0, str_kza, NAT},
           id_jtt: MININT,
           id_rlp:
           ({ id_deb, id_eoc, id_jdr, id_prq, id_uyz, id_vgk, id_nwo, id_cvt,
              id_eef
              |
              str_nph <<: str_pcx$0 }
              (str_ywh$0)),
           id_ysg: FALSE)
          ({ id_hqk, id_lwf, id_uyu, id_dvg, id_ibu, id_qfi, id_tth
             |
             str_dhz$0 = str_bqd$0 })&
        id_dze
        =
        { id_sbp, id_qts, id_kyo, id_vus, id_coa, id_irz
          |
          #(id_vyz, id_akv, id_uke, id_nwq, id_kjl, id_vgh).
          (#(id_aqo, id_lfs, id_qfw, id_mvp, id_srl, id_ngi, id_upi).
           (#(id_mam, id_yrg, id_vvt, id_xcw).({} <<: str_khj)) &
           str_adm$0 : NATURAL) }
        ,
        (str_nyt$0(str_hkp$0))&
        id_mca
        =
        { id_uiu, id_qih, id_tnt, id_boi
          |
          #(id_uld, id_avn, id_sqq).
          (!(id_arr, id_azb, id_wqw, id_thh, id_biv, id_hbp, id_kne, id_ckl).
           (!
              (id_gpn, id_kru, id_vjz, id_sln, id_rsw, id_zwr, id_sih,
               id_nxj, id_lgs, id_vgv).
            (rec
               (id_hsp: str_sfw$0,
                id_udn: "str_eaj",
                id_lvc: str_dun,
                id_tut: INTEGER,
                id_oxd: str_pdv,
                id_afy: str_ddg,
                id_iwb: str_lhl$0,
                id_fif: [])
               'id_aza <
             str_pso$0))) }&
        id_jax
        =
        (str_lij ^
         struct
           (id_vmw:
            bool
              (!(id_hkn, id_trk, id_rva, id_vde, id_izv, id_aom, id_nqj).
               ({} /<<: MAXINT)),
            id_stf:
            { id_cuc, id_hxw, id_spc, id_acw, id_qdk, id_vgf
              |
              not(str_jhz < str_msr$0) },
            id_kxb: str_ufv))
          'id_ads&
        id_aah
        =
        [str_atb$0,
         TRUE,
         str_irm$0,
         {NATURAL,
          {},
          str_svx,
          NAT,
          ({str_jqj, str_ufg, str_sgp, NAT1, FALSE, {}, MININT, str_bjo$0} ^
           {str_sur$0, NAT1, str_fyn$0}),
          (str_gzm$0(str_ubv)),
          str_aao$0},
         MAXINT,
         str_lbh'id_rpd,
         str_bja$0]&
        id_edg
        =
        bool
          (#(id_xid, id_qbf, id_dcn, id_dxf).
           (#(id_elf, id_ddh).(str_lgp >= str_ruj)) or
           str_kfo$0 |-> str_hyr$0 =
           rec
             (id_twl: INT,
              id_skm: str_bsk$0,
              id_scz: str_agj,
              id_pbo: str_nwb$0))
      IN
        SELECT
          {str_mds$0,
           rec
             (id_jfe: str_kqi$0,
              id_olu: str_qhj$0,
              id_odq: FALSE,
              id_esf: NATURAL,
              id_gqe: str_zcj$0)} /<:
          rec
            (id_nqo: str_rrp,
             id_rxc: (prj1(str_lfs)),
             id_cmh: str_yiv$0,
             id_bur: str_okg$0,
             id_spp:
             [rec(id_oxq: str_lqg$0, id_ifk: str_wya, id_uia: str_jey$0),
              str_irk$0,
              str_rkd$0])
        THEN
          id_ncj'id_pey
          :=
          bool
            (((closure1(STRING)) , str_kfc$0) , str_rgn$0'id_dgw <= {STRING})
        WHEN
          str_csc$0 <: MININT or
          !(id_qpb, id_svj, id_gly, id_vzd).
          (not(not({} <= str_cqr$0(bool(STRING < str_wqc)))))
        THEN
          id_uwc'id_ulk
          :=
          [(str_abd$0 +->
            struct
              (id_grq: INTEGER,
               id_bup: [],
               id_vun: str_pis,
               id_pap: str_ryb,
               id_oea: str_pio)),
           MININT,
           {MININT,
            str_xoo$0,
            ([(str_qcq +-> str_isa$0),
              NATURAL,
              str_zhb,
              str_rfb,
              STRING,
              str_zms$0,
              str_oli$0,
              str_gpd]
             ,
             STRING),
            {},
            str_wyx$0,
            str_nxn,
            str_lsj},
           str_ngd,
           str_mji,
           str_fmp$0,
           str_knz$0]
        WHEN
          not
            (not
               (!
                  (id_anr, id_qaz, id_zxu, id_ovi, id_faq, id_gwd, id_qsn,
                   id_kmi, id_paz, id_qiq, id_zrw).
                (str_gpz /: []) &
                str_pvj > str_gom &
                FALSE <<: str_wef) =>
             not(str_yoc < str_ayk$0))
        THEN
          id_ocf, id_wjf, id_kjy, id_dhx
          ::
          (str_pqa
             ({str_rgd$0,
               str_ymr'id_ler,
               str_xpe$0,
               NAT,
               ((son(rec(id_cjn: str_rms$0, id_fpw: str_ldr$0, id_rgn: NAT))) ><
                NATURAL1),
               str_zjt}))
            'id_voa
        WHEN
          !
            (id_tqp, id_waf, id_rhf, id_fnk, id_jjv, id_aic, id_yum, id_oyt,
             id_nrp, id_ltc).
          ([NATURAL, str_zii, NAT1, str_rqm$0, str_ibc$0, str_wsz$0] /:
           str_ncf'id_hhr) <=>
          #(id_xto, id_lsu, id_ynq, id_uar, id_rph).
          (MAXINT <: MININT & str_ltq$0 <: INTEGER)
        THEN
          id_qou 
            (
              right
                ((INTEGER >-> str_ogx)
                   (struct
                      (id_vki:
                       { id_bzw, id_vuk, id_evy, id_fkr, id_kvu, id_icb,
                         id_gvb, id_nyr, id_ykx
                         |
                         str_ype /= str_ekl })
                    ,
                    (str_rwn >+> str_bgy$0)))
            )
            (
              rec
                (id_tqv:
                 struct
                   (id_ljp:
                    {str_tou,
                     {{}, {}, str_cew$0, str_aeo, str_gvy, str_rkz$0},
                     rec
                       (id_mdc: str_zmx,
                        id_xxo: str_sxi,
                        id_wpf: (str_ozq$0 - str_wnx$0),
                        id_olz: str_mgt$0,
                        id_eys: str_iav,
                        id_yvq: (str_rup$0 / str_mzs)),
                     str_sib}))
            )
            (
              %
                (id_uky, id_jdt, id_jue, id_jmk, id_rae, id_bbx, id_rhw,
                 id_sdi).
              (not
                 (!(id_cwx, id_sxu, id_rvx).
                  (!
                     (id_wtc, id_flm, id_jrg, id_bqa, id_dkc, id_yce, id_oza,
                      id_kxp, id_iod).
                   (str_xti$0 >= {str_rll, str_srp$0, str_mcg$0, str_yeu})))|
               rec
                 (id_pxe: MININT,
                  id_ppk: str_nfd$0,
                  id_kzd: {},
                  id_ldp: str_hxd$0,
                  id_lxx: str_fbp$0,
                  id_lnz: BOOL))
            )
            (
              btree
                ({ id_wnx, id_hct, id_ufh
                   |
                   !(id_ygs, id_zmz, id_pow, id_bzv, id_uam).
                   ({ id_rea, id_klt, id_cke, id_vgl, id_xkg, id_jma, id_pio,
                      id_ica, id_dly, id_tra
                      |
                      str_srq$0 = str_mlh$0 } /<<:
                    [(str_wyi$0 , str_hay$0),
                     str_syu,
                     str_ykx$0,
                     MININT,
                     str_ngf$0,
                     str_iqz$0]) })
            )
            (
              { id_tom, id_xwi
                |
                struct
                  (id_tdi: str_ahd$0,
                   id_qex: NAT,
                   id_wsp: ((str_fpu$0 , STRING) /\ str_sil$0),
                   id_jor: str_qgs,
                   id_oym: str_jyq) >=
                rec
                  (id_qth: str_idw$0,
                   id_lks: (str_srj$0 |-> str_qfd$0),
                   id_plo: str_cbx,
                   id_vlt: NAT1,
                   id_lrc: NAT,
                   id_izj: BOOL,
                   id_bjm: str_kms,
                   id_tra: str_xss) }
            )
          
          :=
          {(str_wxe <->
            (INT
               (PI(id_paa, id_bbs, id_beb, id_jqi, id_dks).
                (str_xsq > str_xfy$0| MAXINT)))),
           bool(str_qop >= TRUE => TRUE = str_vpc$0),
           rec(id_uxj: str_ntw, id_xbb: str_agu$0)}
        WHEN
          not
            (#(id_jyq, id_wxg, id_gcv).
             (not
                (#(id_opa).
                 (not
                    (#(id_xsv, id_csk, id_lks, id_fiy, id_kwj, id_xoq).
                     (!(id_ppr, id_bor, id_xyt, id_rlk, id_gab).
                      (str_dzo$0 >= str_ree$0)))))))
        THEN
          id_tqp'id_bum
          :=
          POW
            (rec
               (id_bmm:
                struct
                  (id_goo: str_ohd,
                   id_hzz: str_obj,
                   id_qih: NATURAL1,
                   id_hrx: str_vbw$0,
                   id_crr: str_plw),
                id_phq:
                bool
                  (!(id_cgb, id_clu).
                   (str_wjr /<<: FALSE =>
                    (BOOL = str_fcm$0 & str_wrq /= str_wop))),
                id_vos: str_ljl))
        WHEN
          not
            ({INTER(id_jsb, id_niz, id_zpe). (str_pcm$0 : str_viw| str_pvo$0),
              (str_xwp$0 mod MAXINT)} <<:
             FALSE(iseq(1)))
        THEN
          CASE
            {str_qwv, INTEGER}
              (PI(id_cfx, id_evo, id_mpa, id_cmw, id_hks, id_cyh).
               (str_frh$0 < str_mto| str_jmz -> (tree(FALSE'id_sun'id_deg))))
            OF
            EITHER
              bool
                (#(id_iwk, id_srr, id_sfl, id_jvg, id_lvq, id_csq).
                 (str_dcz$0'id_zcz <=
                  struct
                    (id_lzn: str_dzy$0,
                     id_ery:
                     bool
                       (!(id_dwd, id_ydx, id_eng, id_zss).(str_urr /= BOOL)),
                     id_szn: str_uyh,
                     id_lca: INTEGER)))
            THEN
              id_mou, id_jwu, id_vrg, id_mik, id_aet, id_mkv, id_vjs, id_yua,
              id_gwr :(
                !(id_ovq, id_omv).
                (!
                   (id_jlu, id_rvz, id_wpb, id_amg, id_ahu, id_ivo, id_liz,
                    id_kej, id_ldh, id_kqo, id_dqb).
                 (not
                    (#(id_ped, id_gxp, id_gaf, id_lsy, id_xvw, id_sxr).
                     (#
                        (id_yda, id_pzv, id_qaf, id_jnf, id_kks, id_tim,
                         id_jiv, id_lrf, id_dda, id_gbo, id_cbu).
                      ((str_sut <= str_dzu <=> INTEGER = str_ahn) =>
                       str_ddk$0 < str_aqt)))))
              )
            OR
              [{str_hbz, {}, str_vye$0, str_kbu, str_gua},
               str_mtm$0,
               { id_pfv, id_jdl, id_hmt, id_pmw, id_fgm, id_xmi, id_udy
                 |
                 not
                   (#
                      (id_gkd, id_wet, id_oft, id_elf, id_wbl, id_rna,
                       id_pqx, id_agx, id_fku, id_kqb, id_gqx).
                    (INT < MAXINT)) }
                 'id_rwe,
               (str_eih$0 |-> BOOL)]
            THEN
              id_kpb
                ({ id_xud, id_lul, id_qmi
                   |
                   not
                     (str_xks$0 , BOOL >
                      [(INTEGER , str_ubj), (str_vag$0 |-> str_ivn$0), STRING]) },
                 bool
                   (%(id_acu, id_sxd, id_asp, id_orm, id_ccp, id_bkn, id_cqs).
                    (str_lbt : str_lqe$0| str_ank$0) <
                    str_zcp <=>
                    #(id_zbp).
                    (#
                       (id_ezd, id_zmj, id_pjh, id_nir, id_jcy, id_tut,
                        id_fov, id_uku).
                     (!
                        (id_foy, id_qki, id_uva, id_dlz, id_awn, id_twv,
                         id_gcw, id_sle).
                      (NATURAL : str_fac)))),
                 INTER
                   (id_rzz, id_kkc, id_rgi, id_ogk, id_ine, id_owu, id_egn,
                    id_eaz, id_afg).
                 (str_jlq >=
                  rec
                    (id_eqs: str_pgn,
                     id_ovp: str_qxd,
                     id_exe: NATURAL,
                     id_yrq: str_guv,
                     id_mqt: BOOL,
                     id_cif: MININT)|
                  INTER
                    (id_nfu, id_wli, id_wle, id_mos, id_hzi, id_xeg, id_kmf,
                     id_ezk, id_cuc, id_zcw, id_bsu).
                  (str_rna <<: str_euv & [] <: INTEGER| str_dpf))
                   'id_kry
                   'id_euh,
                 [{ id_kye, id_rix, id_kcc, id_acv
                    |
                    #(id_qex, id_zku, id_bzc, id_ixy, id_unc).
                    (FALSE <: INT &
                     (str_ujb /<<: NATURAL => str_xhh$0 < str_ufb$0)) },
                  (str_xlx /|\
                   { id_pef, id_rww, id_xzi, id_gxn, id_rqi, id_knj, id_yqi
                     |
                     TRUE <= str_qbf$0 })],
                 ((str_ssw$0 ^ str_qmo$0'id_yst)'id_okj'id_ndk
                  |->
                  (right(str_oln , STRING))),
                 (INTER
                    (id_idi, id_nzi, id_zcx, id_spc, id_igd, id_vlv, id_xee,
                     id_mly).
                  (!
                     (id_xbn, id_tab, id_mez, id_zsd, id_ycw, id_lci, id_ldx,
                      id_wsi, id_hex, id_dko, id_ryx).
                   (#(id_qvc, id_rai, id_ppy, id_kni, id_oqs).
                    (not(str_hmm /: BOOL -> str_eqf)))|
                   NATURAL)
                    (MAXINT)))
            OR
              struct
                (id_mwt:
                 {{},
                  str_qvt$0,
                  {},
                  str_czq$0,
                  [],
                  str_wpr$0,
                  bool(str_rxp$0 /<: str_dok)},
                 id_blh: bool(str_gnh >= MAXINT & str_ntz /= str_zaa$0),
                 id_scf: (MAXINT(bool(str_rnm = NATURAL))),
                 id_xtm: str_nvg)
            THEN
              id_bzw 
                (
                  [INTER
                     (id_jbr, id_vrj, id_lje, id_sag, id_czp, id_ypf, id_fzw,
                      id_bvv).
                   (str_jfa$0 = BOOL| NATURAL1),
                   rec
                     (id_cql: BOOL,
                      id_lod: [],
                      id_lye: (rel(str_xxs$0)),
                      id_dsu: str_nne,
                      id_idx: NATURAL,
                      id_jwt: STRING,
                      id_vqr: str_vfn,
                      id_pxo: str_xvp$0),
                   {},
                   str_zhh,
                   str_mmd$0,
                   bool(not(str_ezy$0 = STRING))]
                )
                (
                  { id_ywq, id_iol, id_ztp, id_lad, id_atw, id_xnt, id_wzo
                    |
                    !
                      (id_ukj, id_hel, id_pyf, id_ugz, id_ydu, id_kep,
                       id_ntj, id_qvb, id_ywa).
                    (str_nbr$0 : str_ofr) &
                    not(not(str_tdk$0 >= INTEGER)) }
                  |->
                  PI
                    (id_way, id_kqa, id_ets, id_fhz, id_ghv, id_eal, id_gzb,
                     id_wuw, id_cyo, id_yqi, id_oab).
                  (FALSE > str_fpq| str_bis$0)
                )
                (
                  rec
                    (id_qog: NAT,
                     id_thy: str_eij,
                     id_jjn: str_uxg$0,
                     id_wwb: STRING)
                    (struct
                       (id_rtu:
                        {INTEGER,
                         str_bqv,
                         str_hka'id_lru,
                         str_gpx,
                         {str_vlc$0,
                          str_irf,
                          str_btq$0,
                          NATURAL1,
                          struct(id_kpy: {}, id_nbt: str_hvd$0)},
                         INTEGER,
                         str_dvw$0}))
                )
                (
                  {{str_eql,
                    str_qxh$0,
                    str_dcb$0,
                    str_uln,
                    INTEGER,
                    str_qrs$0,
                    { id_ggy | TRUE /: NATURAL },
                    str_kkw},
                   struct
                     (id_gxb: str_mvt$0,
                      id_mxr: [],
                      id_nzh: str_erk,
                      id_zlz: str_etd$0,
                      id_opg:
                      { id_moh, id_pvh, id_dzo, id_egj, id_yab
                        |
                        not(NAT1 /: str_xzr$0) }),
                   NATURAL,
                   str_lpo,
                   str_hfw,
                   SIGMA
                     (id_cxj, id_pvu, id_ief, id_fnh, id_tdn, id_scn, id_ows,
                      id_hsb, id_pmi, id_khy).
                   (str_ute$0 <: str_ghc| str_zix),
                   str_fnu}
                )
              
              :=
              struct
                (id_ixd: str_fyv$0,
                 id_rnl: INT,
                 id_fyp:
                 struct
                   (id_btp: TRUE,
                    id_zyn: bool(NATURAL <<: NAT1),
                    id_tgj: str_rhx,
                    id_mqi: str_iis$0,
                    id_ckd: MAXINT),
                 id_cfi: str_obo$0,
                 id_smn: str_hoo$0,
                 id_yoe: str_ggw,
                 id_jbi: NAT1,
                 id_jor:
                 rec
                   (id_zqk:
                    { id_onq, id_nfj, id_rlj | not(str_wsb$0 /= str_kcy) })
                   'id_brd)
            OR
              { id_ldb, id_bjy, id_qpw, id_tpz, id_lrf, id_lxe, id_bhn,
                id_prz, id_wnw, id_rpw
                |
                !(id_oxp, id_wbt, id_sqb, id_wlm, id_leg, id_bti).
                (!(id_zyh, id_bnr, id_wqj, id_txa, id_xmd, id_svk).
                 (str_oby <= str_igb) &
                 #
                   (id_ksh, id_xzi, id_yte, id_xwe, id_asb, id_mym, id_iuf,
                    id_oun).
                 (str_blr$0 /: str_tlg)) or
                str_pvj$0 /<: str_kpt }
                'id_slq
            THEN
              WHILE
                not
                  (!(id_vvx, id_fly, id_ncb, id_elq, id_wbs).
                   ([str_ujf, bool(str_apt$0 < STRING), NATURAL1] <
                    bool(not(str_rzw$0 >= str_ipg$0))))
              DO
                id_uiz 
                  (
                    rec
                      (id_pzp:
                       [BOOL,
                        MAXINT,
                        PI(id_fwg, id_whh, id_hol, id_ktm, id_dxx, id_hni).
                        (str_okn$0 > str_svg => str_lkq <<: str_dkm| STRING),
                        str_tsz$0,
                        str_pnl$0,
                        str_vxz$0,
                        MAXINT,
                        str_skc],
                       id_wnh: (str_grs$0(str_uon$0)),
                       id_dtj:
                       %
                         (id_smx, id_hwk, id_bby, id_zhx, id_lkv, id_omj,
                          id_xby).
                       (str_luc$0 < str_qih$0| str_hhg$0),
                       id_tng:
                       rec
                         (id_bnx: str_yih,
                          id_pbb: {},
                          id_bnq: str_kcm$0,
                          id_zev: str_fiq,
                          id_hnx: str_zrc$0))
                  )
                  (
                    {(struct
                        (id_mxq:
                         {str_ggk$0, rec(id_qfg: str_xcy$0), str_oag$0})
                        (SIGMA(id_gml, id_tjk, id_irg).
                         (FALSE <<: NAT1| str_nkk)))
                       'id_vbe}
                  )
                  (
                    [SIGMA
                       (id_xjo, id_voh, id_qgq, id_gka, id_nnb, id_kcj,
                        id_qrs).
                     (#
                        (id_nec, id_uio, id_zpz, id_rbb, id_hxr, id_dhz,
                         id_mqx, id_lkc, id_usx, id_whq).
                      (str_obx = str_gvg)|
                      rec
                        (id_xgt: (union(NATURAL1)),
                         id_djn:
                         (str_nnz$0
                            ({"str_twp", str_edm, str_yss, str_lpt$0,
                              str_wtr$0, {}, str_qau})),
                         id_ctc: str_rks$0,
                         id_rhg: str_eax,
                         id_iji: str_hgg$0))]
                  )
                  (
                    SIGMA(id_ugw, id_vqi, id_cxq, id_rrg).
                    (878 <= INT| str_owp , str_fbk$0)
                    |->
                    struct
                      (id_ets: str_ypl,
                       id_eim: MININT,
                       id_par: (prefix(str_pwf)),
                       id_gfw: INT,
                       id_zbo: str_uom$0,
                       id_jgn: (conc(NATURAL <-> str_bnr$0)))
                  )
                  (
                    rec
                      (id_peh:
                       struct
                         (id_yme: str_mtx,
                          id_fhf: bool(str_vqn >= 105)'id_vkf,
                          id_hfc:
                          INTER
                            (id_pyp, id_szf, id_rzf, id_kgs, id_izk, id_gah,
                             id_zns, id_rox, id_gtd).
                          (#(id_ezc, id_oaf, id_lxu, id_err, id_dyd).
                           (str_xiy /= MAXINT)|
                           str_hzq .. INTEGER),
                          id_ran: str_dje$0,
                          id_uzb: MININT),
                       id_akj: str_cjt,
                       id_wqz: STRING)
                  )
                  (
                    struct
                      (id_mxm: "str_roq",
                       id_yum: str_msp$0,
                       id_kid: str_hck$0,
                       id_wcf:
                       INTER
                         (id_att, id_qsc, id_aei, id_ovv, id_dmr, id_ywk,
                          id_hng, id_sgf).
                       (#(id_zje, id_xiz, id_wrs, id_ohw, id_trj).
                        (!(id_mkn, id_qmd, id_pdx, id_xnn, id_jwg).
                         (not(!(id_hkt, id_hji).(str_ipc > str_bwz))))|
                        bool(str_rqh = str_xaa)))
                  )
                  (
                    rec
                      (id_fyq:
                       struct
                         (id_pwh:
                          SIGMA(id_azv, id_bsc, id_exc, id_zxt, id_dgy).
                          (str_jww <<: str_llh| str_qah$0 --> str_dkp),
                          id_crv: (NATURAL |-> str_qxu),
                          id_typ: (str_bnh$0 |-> TRUE)'id_lxi))
                  )
                  (
                    INTER(id_zhz, id_owi).
                    (INTER
                       (id_tbr, id_ynn, id_mkr, id_uvs, id_vax, id_vhg,
                        id_fby, id_jwb).
                     (!(id_oeg, id_knh, id_jgj, id_ssr, id_izt).
                      (not(MININT = str_hsp$0))|
                      [str_usv, str_ioc$0, str_aan$0, str_ldn$0, str_tcr]) :
                     struct
                       (id_cba: str_vcz$0,
                        id_oyd: {},
                        id_nya: str_jjv$0,
                        id_mci: str_end,
                        id_ikw: str_fbl,
                        id_biu: [],
                        id_bqc: str_uhu$0,
                        id_nbk: FALSE)|
                     str_qrd)
                  )
                  (
                    struct
                      (id_ldh: str_ejl,
                       id_zgk:
                       {NAT1,
                        (str_uik , bool(not(str_xht : MININT))),
                        str_xbd,
                        TRUE,
                        str_qbz$0,
                        (first(bool(str_uwn <<: NAT))),
                        str_iif},
                       id_rxm: str_gth$0,
                       id_dmw: str_fuy,
                       id_pem: str_axy,
                       id_hxn: str_tnk$0,
                       id_dom: str_dkt)
                  )
                
                :=
                PI(id_vrz, id_jpa, id_nyv, id_prx, id_cvb, id_zzx).
                (MININT = str_rjb|
                 {{ id_iaq, id_vpf, id_ehu, id_wne, id_xdo, id_llb, id_iwh
                    |
                    !(id_ojh, id_mhl, id_brj, id_eab, id_acn, id_sqa, id_tyt).
                    (#
                       (id_qkk, id_xcq, id_xgp, id_hqt, id_oue, id_hoh,
                        id_pqk, id_nie, id_raa).
                     (sons({}) = str_aaz)) }})
              INVARIANT
                #(id_irn).
                (not
                   (not
                      (not
                         (!
                            (id_fnn, id_dsq, id_nrd, id_yfu, id_nxj, id_vsq,
                             id_vda, id_nhg, id_lai, id_xcv, id_fkb).
                          (#
                             (id_upr, id_mpd, id_sqx, id_vtl, id_ekb, id_ija,
                              id_hdd, id_mbk).
                           (str_teu >= NAT <=> NATURAL <= NAT1))))))
              VARIANT
                str_qwj
                  (INTER(id_hjv, id_bjp, id_gpx, id_awu, id_dio).
                   ((str_hkt /<: str_gny$0 => str_mzj > NAT) =>
                    not(str_mae$0 /= INT)|
                    str_dhx$0) \|/
                   str_wtm$0)
              END ;
              id_nsv
              <--
              id_qjw
                ((iseq1
                    ({ id_xfm, id_tmj, id_ooo, id_kul, id_jlo, id_fcu,
                       id_ymq, id_zxn, id_jqb, id_mkl
                       |
                       #
                         (id_geq, id_shl, id_fso, id_frq, id_cql, id_kgw,
                          id_tfq, id_jdl, id_gkb, id_wco, id_ghs).
                       (#(id_atj, id_nbu, id_izr, id_phn).
                        (str_als <<: str_udk)) &
                       !(id_jrz).(str_rby$0 /= str_yrm => str_far /: NAT) })),
                 struct
                   (id_ath:
                    rec
                      (id_igy: BOOL,
                       id_hif: str_dix$0,
                       id_zpi: str_wpr$0,
                       id_dwq: 2,
                       id_uvh: str_pbm,
                       id_ouy: str_gdd),
                    id_dps: str_vdp,
                    id_lym: struct(id_fwh: NATURAL),
                    id_twx: struct(id_gin: bool({} <<: str_vbd$0)),
                    id_uvh: INT,
                    id_zwu: (str_thy$0 |-> rec(id_jch: str_tzw))),
                 {struct
                    (id_ybs: MININT,
                     id_bsz:
                     rec
                       (id_xxl:
                        (rec
                           (id_ymv: str_ekl,
                            id_dri: str_ayz,
                            id_jwt: str_oid,
                            id_uid: NAT,
                            id_ucv: str_zaz,
                            id_oed: str_wwl,
                            id_ayq: BOOL,
                            id_wht: str_mhx$0)
                         ,
                         str_mul),
                        id_wnp: str_isu,
                        id_ndn: str_kgz$0,
                        id_myp: str_xih$0,
                        id_vov: str_gjk,
                        id_hse: str_exj),
                     id_xyc: INTEGER),
                  ([STRING, NATURAL1, TRUE, MININT, str_shr$0, str_hsn$0,
                    str_uva]
                     ([]))},
                 rec
                   (id_pww: str_ibe,
                    id_oqg:
                    [bool(not(NAT1 /<: str_fyt)),
                     FALSE,
                     str_uss$0,
                     rec
                       (id_rmw: str_lub$0,
                        id_zqw:
                        { id_crt, id_iqm, id_aou, id_ici | str_rsx < NATURAL1 },
                        id_efn: str_tmo$0,
                        id_ujf: "str_tub",
                        id_tdq: str_upq,
                        id_kjn: str_kzc$0,
                        id_fxd: bool(str_qyt$0 <<: str_xkg$0))],
                    id_bfz: NAT1,
                    id_fnv: str_qeb$0),
                 rec
                   (id_wtc: NATURAL,
                    id_rhm: str_xko,
                    id_ttd:
                    (tree
                       (struct
                          (id_upv:
                           bool
                             (rec
                                (id_zxh: INT,
                                 id_ujd: INTEGER,
                                 id_ccq: str_lif$0,
                                 id_pbg: str_inx$0,
                                 id_qoy: MININT,
                                 id_ahu: [],
                                 id_tzp: str_xqg,
                                 id_xyk: str_gyt) /<<:
                              str_ldj$0 |>> INT)))),
                    id_ymx: {},
                    id_imq: str_dud),
                 (str_ehe
                    (INTER
                       (id_wtr, id_mtg, id_ylt, id_ojq, id_qzn, id_epq,
                        id_qft).
                     ({ id_nvp, id_uyu, id_ckv, id_jxx, id_jmb, id_zxx,
                        id_fja, id_atj, id_wrq, id_cuq, id_zhh
                        |
                        7776 = FALSE } <<:
                      str_hvb$0'id_ahu /\ NAT'id_blu|
                      "str_kvk"))))
            OR
              (((NATURAL1 , NAT) ||
                struct
                  (id_kat:
                   ((str_qyq$0
                       (rec(id_idq: str_nvb, id_xxq: BOOL) |-> str_ygj$0))
                      (str_tww$0)),
                   id_ohs: str_rdx,
                   id_muz: str_jun$0,
                   id_sdj: str_kys,
                   id_klr: str_sez$0)))
            THEN
              id_wte, id_nxh, id_pxy, id_kfj
              <--
              id_veb
                ([NATURAL,
                  struct
                    (id_pmi:
                     rec
                       (id_unk: STRING,
                        id_lco:
                        {{}, str_nwo$0, str_uhr$0, str_bty$0, str_vjn$0,
                         NATURAL, INTEGER, INT}
                          'id_jfa,
                        id_lcv: NAT1),
                     id_qcw: str_rcc$0,
                     id_eys:
                     [str_pms$0,
                      MININT,
                      str_nfs,
                      str_zcq$0'id_lyh,
                      str_axw,
                      str_uln$0,
                      INT]),
                  str_gbv],
                 rec
                   (id_awl:
                    (conc
                       (UNION(id_iry).
                        (not
                           (rec(id_cbz: str_mhu$0, id_qin: str_hfs) > str_abr)|
                         str_cwc$0)))
                      'id_rxk),
                 { id_wyx, id_sgz, id_oah, id_twy, id_obs, id_jsk, id_rck,
                   id_sqn, id_gmm, id_ozt
                   |
                   (str_nbu <= str_ega$0 <=> INT /: str_elg$0) <=>
                   MAXINT /<<:
                   {(str_rac$0 , str_tcz$0),
                    PI(id_qad, id_ekm, id_fqb, id_eof, id_mxz, id_ded).
                    (FALSE : NATURAL1| str_fox),
                    NATURAL} },
                 {{{str_wdw$0, str_xvu$0, str_qta, NAT1},
                   FALSE,
                   str_css$0,
                   [FALSE,
                    str_iju$0,
                    { id_fie, id_bhq, id_ufg, id_jiu, id_ndd, id_fho, id_viz,
                      id_qal, id_lga, id_elg
                      |
                      not(str_qzn$0 = str_kgi) },
                    str_lzb,
                    str_zil$0,
                    INT,
                    INT,
                    str_raj$0],
                   { id_dpu, id_fsj, id_uoh, id_ioc | str_tyy <: str_zub$0 }},
                  MAXINT},
                 rec
                   (id_xjx: bool(str_koc <= str_xhg),
                    id_drh:
                    (bool(INTEGER <: NAT1 => str_fvv /: NATURAL)("str_uxr")),
                    id_kwd: str_djz$0,
                    id_bsh:
                    %(id_gin, id_jgj, id_shb, id_zuz).
                    (str_koa /<<: str_juz$0| { id_wcn | [] = NATURAL1 })),
                 { id_mqb, id_jzn, id_yce, id_pbm, id_fmg, id_bed, id_zpr,
                   id_kqd, id_lyj
                   |
                   #
                     (id_kgf, id_xlg, id_ckt, id_udc, id_ajj, id_xmy, id_kce,
                      id_xnd).
                   (str_vcf /<:
                    {str_rcp,
                     str_qqh,
                     [str_zhs$0],
                     str_zrn$0,
                     str_vag,
                     TRUE,
                     str_oxq} <=>
                    (str_yec = str_san <=> str_uac$0 > MAXINT)) },
                 (rec(id_ukz: (str_dio <-> str_mmn$0), id_cjs: str_oex)
                    (struct
                       (id_klz: (str_uif$0(str_foz$0)),
                        id_wqn: str_qwm$0,
                        id_rqz: str_zyn$0,
                        id_qvq: FALSE,
                        id_sal: MININT,
                        id_qhi: str_lay$0,
                        id_rfe: str_tgq$0)
                       'id_akj))
                   'id_wuv,
                 rec
                   (id_cfl: str_bbp,
                    id_cim:
                    ([BOOL,
                      INTEGER,
                      {NATURAL, str_srw$0, str_zbm, str_lhq$0, str_jza,
                       str_wlg$0},
                      rec
                        (id_cak: str_vni$0,
                         id_ymt: str_dyh,
                         id_eyx:
                         { id_iyn, id_ode, id_lqq, id_kho, id_wvp, id_vgc
                           |
                           str_ijl <: NAT1 },
                         id_vmm: INTEGER)
                        'id_gdo]
                       (str_olx$0))),
                 SIGMA(id_ktk).
                 (!
                    (id_wgq, id_huc, id_ruw, id_vyt, id_svh, id_xgn, id_dxt,
                     id_qqd, id_niy, id_vfq, id_ctu).
                  (!(id_dop, id_cyo).(str_scb$0 /= str_nvw))|
                  union
                    (INTER(id_szu, id_zwx, id_thw).
                     (str_dtw$0 > str_gzb| [] |-> str_zug$0))),
                 struct
                   (id_yrz: INTEGER,
                    id_yzq: MININT,
                    id_toq: TRUE,
                    id_zyk:
                    bool
                      (str_mxh >
                       rec
                         (id_zrz: str_obv$0,
                          id_uvh: str_mdj,
                          id_nnu: str_prx$0,
                          id_yyi:
                          (%(id_ytn). (str_cid$0 = str_tkc$0| str_yqd$0) |>
                           (str_xfe$0({}))),
                          id_haw: str_yyy$0,
                          id_fyu: str_iag)),
                    id_nrl: str_mwk$0,
                    id_vek: str_pwq$0,
                    id_zlc: BOOL))
            ELSE
              id_hxo 
                (
                  %
                    (id_xeq, id_ava, id_tsi, id_lmd, id_jir, id_rbv, id_trh,
                     id_vxh, id_nbv, id_qjf, id_qsf).
                  (!(id_ztk, id_wiy, id_jhf, id_mat, id_ecu, id_prn, id_rpw).
                   (str_uhq$0 <<: str_ckt$0) or
                   (STRING = str_rbt & FALSE <: str_nma)|
                   NATURAL , NAT)
                    ([MAXINT, str_ihv$0, str_nuy, str_hqn$0])
                )
                (
                  UNION(id_kfz, id_fjs, id_qid).
                  (!
                     (id_csu, id_pku, id_hac, id_jgk, id_bkh, id_xuv, id_vzg,
                      id_hft, id_qnq, id_yag).
                   (str_jxf >= str_opx =>
                    (not(MININT : MAXINT) => str_bam >= str_qjq$0))|
                   [str_wca, str_adt, rec(id_yge: NAT, id_nlc: FALSE)])
                )
                (
                  rec
                    (id_snl: MAXINT,
                     id_yoc: str_keh,
                     id_erh:
                     ({ id_orj, id_ufs, id_gxk, id_ueh, id_ljz, id_rzy,
                        id_ksn, id_tql, id_tkk, id_hdn, id_fci
                        |
                        #
                          (id_clk, id_pge, id_abk, id_awd, id_kzo, id_rmb,
                           id_zbm, id_xql, id_obo).
                        (INTEGER /<: str_zam or TRUE /<<: str_uso) } ^
                      NATURAL1),
                     id_swj: str_wgo$0,
                     id_dqt: (str_jnc , str_xtl))
                    'id_rwt
                )
              
              :=
              {str_mcq,
               { id_vqc, id_sih, id_evk
                 |
                 bool(MAXINT /<<: str_bby) /<<: str_znb$0 },
               str_dta$0,
               str_fog,
               str_pcs$0,
               NATURAL1,
               str_xmc$0,
               (rec
                  (id_yuz: str_imb$0,
                   id_rdf: str_cyr,
                   id_onq: str_veb,
                   id_uvz: str_vfz$0,
                   id_bjy: STRING,
                   id_bvf:
                   { id_ure, id_hxj, id_twj, id_qaa, id_tlj, id_fkx
                     |
                     NATURAL <= {} },
                   id_mhm: str_rec$0,
                   id_tzo: str_bxm$0) ^
                NAT1)}
            END
          END
        WHEN
          #(id_ckx, id_mne, id_ipb, id_him, id_ybc).
          (#(id_hgg, id_exx, id_egj).
           (not
              (!(id_lqz, id_urm, id_you, id_cex, id_fyn, id_kce).
               (str_gbm = str_pdw <=> str_xfk <: INTEGER) or
               !(id_dxi, id_mbu, id_ujy, id_cvq, id_cme, id_wce, id_xkg).
               (NATURAL < str_wrm$0))))
        THEN
          skip
        WHEN
          not
            (not
               (not
                  (not
                     (!
                        (id_jjp, id_ggg, id_ecu, id_mim, id_noy, id_hvn,
                         id_chp).
                      (str_lno$0 <= str_gxh => str_vhd <= NATURAL)))))
        THEN
          id_diu, id_kmr, id_brl, id_skl, id_wfu, id_gbi
          :=
          rec
            (id_vlg: str_bob,
             id_och: INT,
             id_ghj: str_eic$0,
             id_kar: str_voc$0,
             id_qvv: str_lnx,
             id_ngr:
             %
               (id_lsp, id_zii, id_wnf, id_crn, id_hey, id_ywz, id_irs,
                id_pgy, id_jeh).
             (str_voo$0 <<: [] <=> str_aab$0 < str_apa$0|
              bool
                (!(id_dub, id_jfy).
                 (!
                    (id_lak, id_jbx, id_qrd, id_ahr, id_jli, id_jnr, id_tfe,
                     id_zkd, id_env).
                  (str_lfb$0 = NATURAL))))
               'id_gxt,
             id_zan: str_hmx$0,
             id_ftg: TRUE)
        END
      END
    END
  END;

  id_grj (id_chj, id_tls, id_lja) =
  LET
    id_kur, id_tmj, id_kdz, id_uur, id_qtu, id_uwn, id_hcw, id_his, id_tun,
    id_ehc, id_vdr
  BE
    id_zkf
    =
    struct
      (id_ujq:
       bool
         ((struct
             (id_oty: str_aod$0,
              id_rcv: str_mux$0,
              id_vqy: str_snj,
              id_rge: str_uli$0) >->>
           str_hbh$0)
            'id_qlf
            'id_biu <=
          FALSE))&
    id_cpj
    =
    ([str_dlg$0,
      str_taa,
      str_zde$0,
      BOOL,
      str_wep,
      NATURAL1,
      str_atc,
      {str_vep, {}, str_pwg, MININT, MAXINT, str_aly, MAXINT, str_ntz$0}] >+>
     (min({NATURAL, NATURAL, str_uyh$0, str_gkf$0, 1, MININT})))
      'id_upm
      'id_ivw&
    id_mtm
    =
    bool
      (not
         (#(id_xme, id_ros, id_hef, id_mck, id_ock, id_wgo, id_lfl, id_iqz).
          (TRUE'id_gkg /= union(str_zkn |-> str_xmf))))&
    id_yfs
    =
    [{ id_dpr, id_jtm, id_bkz, id_iif, id_guy, id_bkr
       |
       str_koq < str_rwq$0 or str_kbk : MININT },
     (str_nun , str_dkb$0),
     rec(id_crr: str_ymv, id_jyq: {}, id_fed: str_brh, id_tbp: str_upp),
     (closure({str_guz, NATURAL1, str_zwg})),
     str_hmu$0]
  IN
    CASE
      {struct(id_obo: str_pqu$0),
       str_cqk,
       (PI(id_gxx, id_gsu, id_vtg, id_jjv, id_grv, id_fio).
        (#(id_vtb, id_whe).(STRING >= NATURAL)|
         struct
           (id_xzo: str_xgl$0,
            id_yrs: NATURAL,
            id_qbx: TRUE,
            id_lfw: {},
            id_yla: FALSE))
        |->
        str_pta$0),
       [str_tie, STRING]}
      OF
      EITHER
        struct
          (id_ehi: str_cqh,
           id_rnu: (NAT >-> str_rhe),
           id_tdc: [str_ted, str_lvz, str_bpf, str_imt$0],
           id_dtk: (str_bxd(str_eop$0)),
           id_wjz: NAT,
           id_wpv:
           (UNION(id_qmi, id_gjk, id_rcb, id_hqr, id_oxv, id_iwk).
            (str_wlg$0 : str_fnx$0| str_zen)
              (str_meg$0)))
      THEN
        LET
          id_qwx, id_tpq, id_prw, id_fyt, id_qok, id_qpx, id_yxy
        BE
          id_wac
          =
          { id_qzz, id_blk, id_qmt, id_xuf, id_vfj, id_epa, id_cus, id_rgv,
            id_atg, id_vcf
            |
            card(str_vxr$0) /<: bool(str_cyf <: MAXINT) }
            (perm(bool(str_cvj$0 >= 0)))&
          id_kkc
          =
          struct
            (id_npg: str_quz,
             id_zue: NATURAL,
             id_aqi: [str_lyj$0],
             id_ggh:
             [(str_beg$0 , 1), str_opx, (str_bwk(MININT)), str_fcv, str_rql],
             id_hcc: NATURAL1'id_ypp,
             id_kzw: str_zdx$0,
             id_zgp: str_cey,
             id_bam: INTEGER) /|\
          str_crs$0&
          id_bln
          =
          { id_yqc, id_ctj, id_qin, id_coq, id_kqk, id_spp, id_xbp, id_nok,
            id_oos, id_hcz, id_zft
            |
            rel
              (struct
                 (id_ggw: str_ler,
                  id_rph: str_dqo,
                  id_szl: NAT,
                  id_uyg: str_nps,
                  id_gco: str_olz,
                  id_yja: str_ker,
                  id_kpp: str_azs)
                 (str_cnv$0)) <=
            struct
              (id_tzg: str_wqi,
               id_ukk: INT,
               id_rhm: str_rfh$0,
               id_tjv: BOOL,
               id_mdw: str_bdo$0) <=>
            INTEGER < NAT }&
          id_dki
          =
          SIGMA
            (id_pwt, id_cdm, id_nuq, id_bww, id_idf, id_tun, id_cmt, id_mao,
             id_den, id_vbl).
          (!(id_yak, id_cyu, id_bzh, id_vpf, id_bbz).
           (#(id_mxu, id_oql, id_lrl, id_tkv).(str_rcj > TRUE) <=>
            #(id_prl, id_thn).(str_oyi$0 <<: str_wdf$0))|
           MAXINT \|/ str_cyj)&
          id_prd
          =
          STRING
          ,
          {str_ata,
           str_hej,
           INT,
           MAXINT,
           str_viv,
           bool(str_inh$0 > str_xyl$0),
           struct
             (id_syu:
              struct
                (id_wcy: str_jkw,
                 id_xqt: NATURAL1,
                 id_vxr: str_bqi,
                 id_ckn: str_tsv,
                 id_aac: str_eum$0,
                 id_bfd: INTEGER,
                 id_ctc: str_gvc,
                 id_wwm: str_krq),
              id_llb: bool(str_bov$0 < {}),
              id_nyu: str_lds$0,
              id_jef: NAT1,
              id_kui: str_iut$0,
              id_kdy: str_sqd$0,
              id_rnz: str_mjt,
              id_bks: str_vmu),
           BOOL}
            'id_srq&
          id_rgi
          =
          rec
            (id_csf:
             ((prj2(NAT , { id_lpf, id_cmb, id_kxt | INTEGER < str_dlp }))
                (FALSE)),
             id_kqn: (bool(MININT /<: str_mwr$0) |>> str_ker$0))&
          id_yxa
          =
          card
            (rec
               (id_enr: str_dzn,
                id_ubi: str_fim$0,
                id_awg: str_emh,
                id_vii:
                {str_hcf$0, (INT(NATURAL)), str_rhp, [], str_olj, str_coe},
                id_pen: str_zzm$0,
                id_kjn: str_eym,
                id_mcm: NAT,
                id_bru:
                INTER
                  (id_esc, id_mlh, id_dws, id_tib, id_dfk, id_vuw, id_xjn,
                   id_jxi, id_pfc).
                (str_vnt$0 /<<: str_tue|
                 {MININT,
                  (MININT(4)),
                  str_cpx$0,
                  str_owb,
                  str_qdz,
                  str_jgv$0,
                  str_jvi,
                  NATURAL})))&
          id_awh
          =
          str_sez
          |->
          SIGMA
            (id_wkv, id_lkc, id_qgc, id_fao, id_qvk, id_kbi, id_evu, id_fzh,
             id_xwi, id_tlp).
          (not(str_xox /= str_xny)|
           {str_fcj$0, (str_gok |-> rec(id_vmh: bool(MININT = str_vyu)))})
        IN
          PRE
            {} /<: str_vbv =>
            str_syk$0 >
            (UNION(id_bgw, id_qjy).
             (str_lhq : str_xhu$0| str_yxg -->> NATURAL) ||
             bool
               (!(id_zzq, id_woy, id_zhw, id_hil, id_jba, id_lzp).
                (str_nqk <: str_opk)))
          THEN
            SELECT
              #(id_tof, id_vxu, id_uyi).
              (str_ktq$0 = str_upj &
               #(id_myt, id_jpd, id_ipy, id_uib, id_qjb).
               (#(id_tyk, id_kno, id_eef, id_tnq).
                (#(id_elj, id_gzy, id_crg, id_jyu, id_emc, id_wvi, id_yfj).
                 (MININT /= INT))))
            THEN
              id_sjt, id_ubp, id_rmz, id_atr, id_nmt, id_fod, id_lft, id_yyx,
              id_kse
              <--
              id_plh
                ((union
                    (SIGMA(id_akg, id_lgo, id_bbw).
                     (!(id_bgm, id_yyl, id_jxy, id_hik, id_auu, id_tgz).
                      (#(id_vmg, id_bxg, id_ybm).(str_fwy /<: str_rqt) or
                       NATURAL > str_egc)|
                      str_phz(str_bnq))))
                   'id_wku,
                 rec
                   (id_bun:
                    {struct
                       (id_sxm: str_ehr,
                        id_tsh: str_foy$0,
                        id_myr: str_alu$0,
                        id_jiw: NATURAL1,
                        id_euu: str_ohh,
                        id_idm: str_lvf),
                     {str_oqq$0,
                      str_pyg,
                      str_gfg$0,
                      str_iwj$0,
                      (str_hfj$0 , str_jth),
                      str_pgv$0,
                      str_nsu},
                     INTER
                       (id_cmr, id_ufb, id_bbz, id_dba, id_njt, id_nwi,
                        id_mob).
                     (#(id_krf, id_qjs, id_smc).(str_ose$0 < INT)| str_bks)}),
                 { id_cbb, id_mds, id_orl, id_azr, id_nvu, id_rjw, id_cxj,
                   id_php
                   |
                   rec
                     (id_nrf: str_pjw$0,
                      id_ssy: (str_tog$0(str_gag$0)),
                      id_vxq: NAT1,
                      id_aja: str_izd$0,
                      id_aqz: str_ddu$0,
                      id_ked: str_ivv$0,
                      id_ayl: str_utk)
                     'id_hdd <=
                   rec(id_qcf: str_ewu$0) mod TRUE },
                 %(id_vxb, id_qol, id_azq).
                 (not(not(str_vxx /<: NATURAL1))|
                  rec(id_sic: INT, id_efk: str_yxz$0, id_skk: str_bww$0)
                  |->
                  BOOL'id_buu),
                 ({ id_jmz, id_gjj, id_shp, id_svd, id_lba, id_bmy, id_mfw,
                    id_jfr, id_umd
                    |
                    not
                      (!(id_dmi, id_rbn, id_bzm, id_nmr, id_knl, id_iok).
                       (!(id_lkw, id_fud, id_alq, id_tij, id_psi).
                        (str_mpt$0 <<: 4))) }
                  |->
                  [str_sex, str_wbr$0, str_dcl, BOOL, str_qtn, str_kcz])
                   'id_rib,
                 { id_yqt
                   |
                   #
                     (id_ijg, id_znz, id_xel, id_klx, id_nzq, id_uma, id_nkg,
                      id_bos, id_ddp).
                   (!
                      (id_tuj, id_rqo, id_qsx, id_sjv, id_fms, id_cod,
                       id_npr, id_vdf).
                    (str_rof$0 > STRING <=> {} = str_tpz) &
                    not(str_xbw /<: str_smo)) },
                 [str_tuj$0,
                  str_uox$0,
                  BOOL,
                  str_ljs$0,
                  rec
                    (id_uyk:
                     %(id_cmy, id_lku). (not([] /<<: str_jjg)| ran([])),
                     id_gms: str_kbt,
                     id_xpu: str_sxl$0,
                     id_mip: (size(str_jgr$0)),
                     id_vde: str_sns),
                  rec
                    (id_dia: str_oro$0,
                     id_ujt: INTEGER,
                     id_uqn: str_jxp$0,
                     id_zcq: NATURAL,
                     id_hrq: {})]) ;
              id_xnq, id_mgk, id_ekj, id_lej, id_lhb, id_bzv, id_ztd, id_fts,
              id_pin, id_kxr :(
                NATURAL /:
                rec
                  (id_kmx: (inter(TRUE)),
                   id_cyw:
                   (id
                      ({ id_flv, id_xiy, id_gfe, id_dib, id_ioh, id_cfw,
                         id_juz, id_bwo
                         |
                         str_viz$0 /: str_hyz$0 })),
                   id_qso:
                   INTER(id_vob, id_nox, id_mru).
                   (str_tqv /: {}|
                    struct
                      (id_cqq: FALSE,
                       id_uld: str_yng,
                       id_hhu: NATURAL,
                       id_vqs: [])),
                   id_bzr: INTEGER,
                   id_xal: INT,
                   id_zmw: str_ubf)
              )
            WHEN
              !
                (id_srr, id_jyk, id_imy, id_eml, id_hfq, id_bye, id_ghj,
                 id_udx, id_ygi).
              ((str_nlk /<<: str_jmr$0 <=> str_loj$0 /<<: str_sfy$0) <=>
               not(not(TRUE >= str_lqw) <=> str_zax /<: str_ziw))
            THEN
              id_qtu
              :=
              PI(id_vzg, id_tkb, id_mel, id_eav).
              (not
                 ({ id_vjy, id_ccx, id_eig, id_bqq, id_ubx, id_izv, id_uql
                    |
                    str_dkw = [] or str_dws <: str_eij$0 }
                  ,
                  BOOL >=
                  str_yzs)|
               SIGMA(id_cyn). (TRUE <: str_taw| MININT))
            END
          END
        END
      ELSE
        id_yqx'id_zza
        :=
        ({ id_rbf, id_pvp, id_fgr | str_pwc$0 = 154 } |> str_mqw)
          ({ id_isg, id_eqr, id_oyv, id_mzp, id_tit, id_aot, id_hry, id_edw
             |
             (not(str_ohx$0 /<: str_xlj$0) <=> {} >= str_gpk) or
             str_fvb$0 < str_gpg }) ||
        id_zdu, id_vbp :(
          str_apv$0 /:
          bool
            (#(id_ukh, id_dmw, id_beh, id_pof, id_tfm).
             (!
                (id_wwo, id_dyf, id_tzl, id_kqe, id_pat, id_vxe, id_ggy,
                 id_dce, id_szi, id_sdx, id_hhg).
              (not
                 (!(id_uqd, id_ncx, id_fpn, id_jod, id_kew, id_vbk).
                  (INTEGER < NATURAL1 => str_wnp <: NAT)))))
        )
      END
    END
  END;

  id_trk, id_blf, id_btl, id_dex, id_cxh, id_ygv, id_zdl, id_nsu, id_mwq,
  id_ooc
  <--
  id_uzy
    (id_gsa, id_yle, id_num, id_pds, id_kso, id_gdi, id_bnw, id_rmv, id_kqz)
  =
  IF
    !
      (id_kzb, id_zqa, id_ail, id_eqq, id_eck, id_zrg, id_oyh, id_yqy,
       id_xua, id_vlf).
    (not
       (!(id_yae, id_odc).
        (#(id_tky, id_mez, id_pvj, id_zre).
         (bool(#(id_zdm, id_pns, id_atz, id_nhx, id_qpj).(str_xyi$0 : {})) >
          FALSE))))
  THEN
    id_dwx 
      (
        str_vtx
          ({str_zge, str_xrh$0, NATURAL, NAT1, MININT, str_zgo, str_wic,
            str_ykh$0} /\
           { id_ril, id_amg, id_aen, id_wed, id_vwo
             |
             not(not(str_rdd$0 >= MININT)) }
             'id_kze)
      )
      (
        SIGMA
          (id_cum, id_rro, id_oqb, id_dqo, id_isk, id_qip, id_crc, id_rtp,
           id_xdj).
        (!(id_txb, id_cjo).
         (not
            (not
               (str_tdj$0 = str_yxv &
                !(id_yjd, id_aws, id_pmd).(str_hxn <: str_sgn))))|
         str_sku'id_ebe)
      )
      (
        { id_oda, id_otb, id_chc, id_xek, id_dln, id_lta, id_ftl, id_hsi,
          id_wjw, id_huo
          |
          not
            (#(id_xyz, id_mnj, id_mzu, id_lnd, id_kng).
             (#(id_gxr, id_zch, id_arn, id_apq, id_xvl, id_stu, id_urh).
              (#
                 (id_wak, id_ylh, id_oki, id_zdk, id_sjn, id_pmz, id_bag,
                  id_pnj).
               (str_ark --> NATURAL < str_ppa)))) }
      )
      ( bool((str_wzd'id_skx(btree(str_yrn$0'id_lmj)))'id_gef <= str_bsh$0) )
      (
        bool
          (str_val$0 /<<:
           { id_pmx, id_xhg
             |
             !(id_hie, id_ykf, id_emr, id_lgg).
             (not(not(str_bek = INT)) or str_kkg$0 <= str_tyq) })
      )
      (
        const
          ((conc
              (PI(id_guv, id_rib, id_yal, id_fqo).
               (#(id_jdo, id_wba, id_cfo, id_nkf, id_vji, id_jik).
                (str_vxi /<<: str_hph$0)|
                iseq1(str_hbr , NATURAL1))))
             'id_ika)
      )
      (
        rec
          (id_zkh: str_ujf$0,
           id_atk: str_xtz,
           id_omo: TRUE,
           id_fma: MAXINT,
           id_gmg: MININT,
           id_zrb: { id_jnp, id_oxl, id_ijq | BOOL /: str_hsq$0 },
           id_wmk:
           (STRING
              (struct
                 (id_jjg: INT,
                  id_sej:
                  [{str_ykr, INT, str_fke$0, str_euq$0, str_hni$0, STRING,
                    str_ewk$0, BOOL},
                   NATURAL1,
                   str_oeu$0,
                   FALSE,
                   (bin(str_ksx$0))],
                  id_ohm: str_hsl))))
      )
      (
        { id_ond, id_fsa, id_zdw, id_xjf, id_aee, id_qng, id_luj, id_vif,
          id_gnf
          |
          struct
            (id_jgk: str_pyg,
             id_nxm: STRING,
             id_ahh: str_wzw$0,
             id_mjx: str_xoz$0) /=
          NATURAL1 &
          ((not(str_bin$0 <<: str_nuj$0) or str_vji /<: str_ubc) =>
           str_lnx < FALSE) }
      )
      (
        {bool
           (#(id_eus, id_hhm, id_cfd, id_hln).
            (iseq
               (str_kbm$0 /\
                { id_kiv, id_onk, id_xjk, id_zmp, id_vwo, id_xsj, id_ryr,
                  id_ese, id_lbd, id_rze, id_pcc
                  |
                  str_xwv$0 /<<: NAT }) :
             str_nvm))}
      )
    
    :=
    [bool
       (str_zog$0 < str_cmz$0 &
        !(id_aam, id_dca, id_vvu, id_tqy).(not(not(str_phy$0 <= str_abs$0))))]
  ELSIF
    #(id_dfl, id_ymu, id_ymj, id_nol, id_wcc, id_obj).
    (str_kiy /: str_see$0 &
     bool
       (#
          (id_arf, id_pbj, id_jxd, id_dpc, id_xdi, id_jqk, id_biu, id_wjq,
           id_ret).
        (str_mhn$0 <= str_eqj$0))
       'id_vkd /:
     {MAXINT, str_dth, NATURAL, str_bdu$0})
  THEN
    id_tlk, id_uqn, id_ypz, id_ugi :(
      dom
        (struct
           (id_nzh: str_lrq,
            id_tda: bool(not(TRUE <: INT)),
            id_wqg: str_hei,
            id_gkm: str_dhz,
            id_nkw: str_krv,
            id_rmr:
            struct
              (id_yjw: str_otf$0,
               id_dxa: str_wpo,
               id_hxr: MININT,
               id_pgv: str_unn$0,
               id_fiz: str_ope),
            id_bib: str_snw$0)) <<:
      MININT ** str_ojx
    )
  ELSIF
    #(id_zfx).
    (SIGMA(id_pyk, id_ehz, id_oms).
     ((NATURAL : MAXINT => str_vsm$0 < str_gei$0) => str_tdx$0 /= str_utt$0|
      TRUE)
       (str_aoz) <=
     TRUE >-> str_jjz$0)
  THEN
    id_fnl, id_kgv, id_xso, id_lou, id_wag, id_vmy, id_sig, id_mfr, id_oxt
    :=
    str_qap$0
    ,
    {%
       (id_bic, id_aaf, id_cqn, id_pti, id_flv, id_wtj, id_wlo, id_ivn,
        id_gjz, id_dkq).
     (#(id_tre).(str_ibf : str_grb)|
      PI(id_uhf, id_qme, id_wed, id_sxb).
      (#(id_tpe, id_ufv, id_jpg, id_frz, id_qwx).(INT /: MAXINT)| str_wpd$0))
       'id_qji}
  ELSIF
    str_bcg /<: MININT &
    !(id_ncp, id_ctw, id_fwv, id_ayk, id_avs, id_omk, id_wfu).
    (str_gus /: str_sbu) &
    !(id_ukr, id_sil, id_ssr).(str_gtp$0 = str_ido$0 , BOOL)
  THEN
    CASE
      [str_ekb$0,
       str_ijt$0,
       ((rec
           (id_bas: str_mug$0,
            id_rjx: str_nfm$0,
            id_nem: str_xhi,
            id_znf: str_flh,
            id_oho: MAXINT,
            id_kpx: str_qms$0)
           (rec
              (id_sas: str_gwb,
               id_maj: FALSE,
               id_nbl: bool(str_pzk$0 > str_vgm$0),
               id_daq: str_cqo$0,
               id_kbk: str_ycr)))
        ,
        str_hcw)
         'id_mxk]
      OF
      EITHER
        (str_xcj$0
           ([MAXINT,
             %(id_mrd, id_mqs, id_oqv, id_gos, id_ttx, id_tzl).
             (str_hus$0 <<: {} or str_dar$0 >= str_hme|
              [str_tux, str_yls$0, str_scn, NAT1, str_qis, str_rwb$0, str_xyq]
                'id_ffy),
             str_gqa,
             str_jdt,
             FALSE,
             struct
               (id_lvw: NAT1,
                id_tkm: str_jmu$0,
                id_lcm: str_thp,
                id_nku: str_scf,
                id_pdx: str_eco$0),
             str_wze$0,
             str_emy]))
      THEN
        id_qkh, id_ttj, id_dbs
        <--
        id_mgq
          ((str_jve
              ([str_zas$0,
                str_vvd$0,
                ((str_lee ; NAT)),
                MAXINT,
                str_qlu$0,
                { id_yrt, id_vnu
                  |
                  !
                    (id_tar, id_zmd, id_bxp, id_bpx, id_dtb, id_tgu, id_lzg,
                     id_hzl, id_wef).
                  (str_dgj <= str_ekg <=> str_yst <<: NATURAL) <=>
                  str_vir /= str_jxy$0 }])),
           (([NAT, INTEGER, str_tuu] \/ str_jnt) <+
            PI
              (id_kxo, id_psc, id_aot, id_hhx, id_vtl, id_ezt, id_dti,
               id_wvu, id_sfs, id_paf).
            (not(str_mhi > str_xok)|
             bool(NATURAL1 > str_rzf$0 & NATURAL1 > str_chk))),
           [rec
              (id_qip: { id_ywq | FALSE /<<: str_ora },
               id_iuz: (conc(NATURAL1)),
               id_pih:
               UNION
                 (id_zem, id_pvq, id_ebo, id_ueh, id_elc, id_aes, id_blb,
                  id_kzv, id_vbg, id_wzj).
               (NATURAL1 <<: str_fyw$0| 7),
               id_enp: bool(54 >= STRING)),
            NAT,
            str_qmz$0]
             'id_abj,
           (iterate
              ([str_eyb$0, NATURAL, MININT]'id_vow
               ,
               INTER(id_kfx, id_fck, id_cip, id_djg, id_duo, id_rfz, id_jvi).
               (str_ixn = str_gyb| NATURAL1 |-> str_psw$0)
                 'id_yol)),
           {[TRUE, NAT, str_cia$0, str_qop]'id_skr,
            SIGMA(id_ier, id_qsn, id_pne, id_dis, id_gsf).
            (#(id_jxo, id_pji, id_fyd, id_tcy, id_rar).(INT > str_ckt)|
             bool(str_xjr >= str_oed$0))
              'id_cqp},
           {INTEGER,
            str_iup,
            str_bqu$0,
            str_zfc,
            {[], INTEGER, str_uhv, str_hnj, MAXINT, str_xir},
            str_saf$0,
            bool
              (not
                 (#(id_rxk, id_fvg, id_uwa, id_lsf).
                  (str_evy$0 /<<: str_wef$0)) =>
               MININT <: str_xvd$0),
            MAXINT},
           { id_ezo, id_nqp, id_yqo, id_wms, id_lps, id_efh, id_hpa, id_snw
             |
             not
               (!(id_pwe, id_nrq).
                (str_twv$0 < str_fjq & str_nnx$0 /= str_utj$0)) &
             #
               (id_sqi, id_edo, id_rfl, id_uup, id_buf, id_bty, id_yat,
                id_xgt, id_isd, id_uik, id_dic).
             (str_wgd <= str_uga$0) })
      OR
        {rec
           (id_zjk: str_xgr,
            id_wba: 6124,
            id_usm: NATURAL,
            id_aut: str_ods$0,
            id_ikg: str_gcv,
            id_tyy: str_zgu$0,
            id_mtl: str_rdp$0,
            id_rlv: TRUE),
         ({INT, NAT, rec(id_yac: MININT, id_gue: str_gfn)}
            (%
               (id_uni, id_bup, id_luy, id_hwy, id_owk, id_muh, id_sut,
                id_iux, id_vsq, id_yux).
             (MAXINT <: []| MAXINT))),
         FALSE,
         str_whn$0,
         TRUE,
         (str_yyg |>> str_ivw),
         str_skk$0}
      THEN
        id_fkb'id_rzd
        :=
        {{INTEGER,
          str_ufm,
          str_ppa$0,
          BOOL,
          %(id_njx, id_bjh, id_kfv, id_prt).
          ([] < NATURAL|
           SIGMA
             (id_pnz, id_mmj, id_lpj, id_ynl, id_bpi, id_xpw, id_skr, id_vgp,
              id_bda, id_yjb).
           (str_gkz$0 < str_cnn| str_wtn$0)),
          str_idc,
          str_esk,
          STRING},
         BOOL,
         rec(id_knb: [str_fbf], id_wfd: NAT1, id_fsv: NAT1),
         str_cfm,
         str_igh$0}
      OR
        PI(id_lrl, id_yik, id_nok, id_vtz, id_vqy, id_oin, id_mbk).
        (str_ozu , str_vey$0 < str_kaz|
         bool(str_qlx$0 /: str_ucf$0(str_oko$0)) |-> str_wng)
      THEN
        id_gvi
        <--
        id_rdn
          ([(str_bzy$0 >->>
             ((iterate({(right(str_kun))})) ^ (str_bkl(str_cye))))],
           bool
             ({ id_sey, id_sne, id_kym, id_yqn
                |
                str_yxj$0 >=
                [str_gwy$0, str_kko, NAT1, str_eij, NAT1, str_hhs, str_ltx$0,
                 str_nun] } >
              bool
                (!(id_axx, id_ruv, id_xol, id_cek, id_qdd).
                 (str_gqz /<<: str_xui))),
           ((rec(id_efl: TRUE, id_ldo: TRUE, id_cef: {str_cho})(str_mzs)) |>>
            (succ(str_dhc$0 \/ ((str_olf || str_lta$0))))),
           bool
             (str_atf /<:
              %
                (id_pnt, id_vjx, id_aje, id_rmh, id_jsv, id_imf, id_rpm,
                 id_fxs, id_tlt).
              (not
                 ((str_dnf$0 <: str_the$0 & str_dhl > str_atb$0) <=>
                  str_gzj$0 /<<: str_hvj)|
               str_jqd |-> MAXINT)),
           (struct
              (id_cyn: str_lkz,
               id_olz: str_dmg,
               id_wwx: [],
               id_jkf: str_rzm,
               id_ibk: str_gla$0,
               id_ejf: NATURAL1,
               id_lmg: str_bbm$0)
              ((str_jsk |> (str_pyx(str_kqz)))
               |->
               (iseq1(str_xfa$0 |-> str_ehb$0)))),
           (conc
              (INTER
                 (id_wey, id_zve, id_tkf, id_ahk, id_shn, id_hfl, id_kyd,
                  id_nli).
               (str_iev$0 : str_yue|
                {NATURAL,
                 str_vmy,
                 str_adc,
                 {(rec
                     (id_xnu: str_uwf$0,
                      id_xsf: NATURAL1,
                      id_ygm: str_ibd$0,
                      id_idm: (str_lul$0 , str_wvq))
                     (str_zrj$0)),
                  {},
                  BOOL,
                  str_olk$0,
                  MININT,
                  TRUE,
                  str_oad$0}}))),
           (str_xne
              (bool
                 (!
                    (id_uud, id_ygl, id_sev, id_xpu, id_xrr, id_rtm, id_ais,
                     id_viy).
                  (!(id_gjh, id_gli, id_kfm, id_brw, id_ycs, id_cil).
                   (#(id_kgr, id_vcc, id_jnc, id_djt, id_vmy, id_nkt, id_cra).
                    (str_yaa$0 /: TRUE)) =>
                   FALSE = str_cyp)))),
           (bool(str_aap >= NAT1)
              ({rec(id_xgp: str_yzm),
                MAXINT,
                MININT,
                { id_hsn, id_zbd, id_qpq
                  |
                  !(id_wgb, id_ecz).(not(str_bpx$0 /: str_gyc$0)) },
                str_osv,
                str_lir})),
           (struct
              (id_ols: str_ntr$0,
               id_vjt: [],
               id_kpe: str_bln,
               id_zzo: INTEGER) +
            { id_csg, id_pts, id_gax, id_kns, id_xyk, id_otj
              |
              #(id_uhs, id_xlq, id_ngs, id_dln).
              (!
                 (id_qhf, id_iek, id_lte, id_iwg, id_itx, id_kbq, id_ckk,
                  id_oro, id_ihx).
               (#(id_irz).(str_rdh$0 > NATURAL1) <=> str_emh <: str_inm$0)) }),
           ((NAT1
             ,
             rec
               (id_zqv:
                (union
                   (bool
                      (!
                         (id_wkk, id_ckw, id_tej, id_swf, id_xtt, id_fjn,
                          id_vjl, id_upw, id_ypl, id_haj, id_nmr).
                       (str_exw$0 > NATURAL)))),
                id_qfd: str_srf$0,
                id_hoy: {},
                id_zzn: MAXINT,
                id_wgu:
                { id_bta, id_ejw, id_dkh, id_ldb, id_grw, id_ajp, id_wps,
                  id_nfy, id_xbh, id_yyo, id_rdu
                  |
                  str_kjs$0 /<: str_ycw$0 }))
            ,
            str_eed$0))
      OR
        rec
          (id_ofm: str_tmm,
           id_chz:
           rec
             (id_eev: str_tjn$0,
              id_scl: str_exv,
              id_hrg: str_smc,
              id_ifu: str_xau$0,
              id_uco: "str_vjt",
              id_ndi: str_zpj$0,
              id_xgy: BOOL),
           id_rea: MININT,
           id_qlk:
           struct
             (id_fzh:
              UNION
                (id_yzl, id_kkg, id_vxd, id_emx, id_isy, id_zsp, id_ztm,
                 id_nou, id_bcn, id_dsv, id_bbm).
              (MAXINT /= NATURAL| str_ixc$0),
              id_ygq: str_slc),
           id_xfd: [(NATURAL , str_sdt)'id_bez],
           id_nun: str_fuo$0,
           id_mys: str_asw$0,
           id_nwo: INT)
      THEN
        id_ydw 
          (
            rec
              (id_uzs: (ran(BOOL , str_krj)),
               id_dxn:
               { id_dtd, id_wsb, id_nrr, id_uxl, id_ssj, id_pnk, id_vhj,
                 id_oml
                 |
                 !
                   (id_jpa, id_whm, id_uoq, id_gdp, id_trp, id_gew, id_pem,
                    id_wap, id_czc).
                 (#
                    (id_ddu, id_ksr, id_myh, id_hsr, id_vgk, id_bes, id_fms,
                     id_etp, id_kdl).
                  (not(str_yup$0 <: TRUE))) })
          )
          (
            rel
              ({bool(TRUE < str_ouz),
                str_bhv,
                str_vzs$0,
                str_cuw$0,
                str_zbg$0,
                INTER(id_isq, id_idg, id_rot, id_hgv, id_qgg).
                (str_oky <= FALSE <=> str_bnd$0 > NAT1| str_ure),
                str_xad$0,
                str_rcz$0}
                 'id_vws)
          )
          (
            bool
              (!(id_rdn, id_uxb, id_ita, id_pqm, id_whm).
               (mirror(str_han$0) /:
                rec
                  (id_xch: str_ugm$0,
                   id_orw: str_oer,
                   id_myr: str_vdj,
                   id_uwt: str_urt,
                   id_gqa: str_rap$0,
                   id_alr: str_vel,
                   id_lbw: NATURAL1) <=>
                (str_dtv < str_eoq & str_nyq$0 /= str_qen)))
          )
          (
            (rec
               (id_iwc:
                rec
                  (id_fhg:
                   struct
                     (id_qck: str_zte,
                      id_gbl: str_vnt,
                      id_ftm: str_vof,
                      id_wxl: str_klf$0,
                      id_iqh: str_qys$0,
                      id_klo:
                      struct
                        (id_lmy: BOOL,
                         id_zdw: str_tla,
                         id_ypt: str_ute$0,
                         id_rlm: INT,
                         id_bon: INT,
                         id_uzl: NAT,
                         id_qtp: str_wuu$0,
                         id_eqw: NAT1)),
                   id_soi:
                   rec
                     (id_nzt: [], id_uxh: NAT, id_lwn: str_fcm, id_fiu: BOOL))) <<|
             str_aau)
              'id_ewc
          )
          (
            {rec
               (id_eeq: STRING,
                id_ztu: str_ppo$0,
                id_eyy: (str_idq$0 +-> str_bvj$0),
                id_orw: str_qxq,
                id_aao: MININT'id_gcw,
                id_dbo: BOOL),
             %(id_obw, id_uak, id_kfr, id_gro, id_uuv, id_tma).
             (rec
                (id_pyj: str_kvd$0,
                 id_vrf: MAXINT,
                 id_ksf: str_smf,
                 id_ayh: NAT1,
                 id_ryl: INTEGER,
                 id_uor: {},
                 id_zsl: str_dkc) <=
              []|
              NAT1),
             str_kkh,
             NATURAL1,
             str_olu,
             BOOL}
          )
          (
            { id_xan, id_hbp, id_kam, id_bko, id_mnf, id_iym, id_jic, id_ctp
              |
              #
                (id_zcj, id_edu, id_kjb, id_jow, id_qzg, id_mve, id_kni,
                 id_esz, id_yhy, id_sct, id_nqz).
              (#
                 (id_qiz, id_xls, id_hqf, id_vqh, id_lzm, id_mxj, id_idi,
                  id_oem, id_qpp, id_nwf, id_tzc).
               (not
                  (str_xsl$0 , str_guk /<:
                   {MININT, str_wut, NATURAL1, str_xlb$0, str_zdt}))) }
          )
          (
            bool
              (#
                 (id_jsf, id_urf, id_wuf, id_oer, id_ccd, id_myt, id_rel,
                  id_psy, id_pbn, id_vav).
               (str_tac$0 /<<: TRUE =>
                #(id_edj).
                (struct
                   (id_zns: [],
                    id_gzs: STRING,
                    id_wvk: str_xrk,
                    id_adv: NAT,
                    id_xpf: str_nry) /:
                 str_syb$0)))
              'id_jbu
          )
          (
            rev
              (((INTEGER(str_osl)) <-> str_pbk$0)
               |->
               rec
                 (id_ifa: NAT1,
                  id_gos:
                  bool
                    (!
                       (id_vrm, id_zno, id_hve, id_mgm, id_geh, id_hzp,
                        id_gwj, id_sts, id_stm, id_vgn).
                     (str_foc$0 /= str_log$0))))
          )
          (
            ({[NATURAL1,
               str_avb,
               SIGMA(id_hix, id_ytu, id_rnx, id_skd, id_gmh, id_lcr, id_sqw).
               (str_jjo < str_ldm$0| str_unf),
               str_hvv$0,
               str_bpu]}
             |->
             (str_eat
                (PI(id_edq, id_kmi, id_aqi, id_ppb, id_tyq, id_jew, id_zxh).
                 (str_qmk <= BOOL| NATURAL1))))
              'id_afe
          )
          (
            struct
              (id_ccn: str_mei$0,
               id_vet:
               [rec(id_rtk: str_kkm$0, id_ozo: str_xmr, id_vkg: str_fmm$0),
                (str_tio$0 |-> str_fxq)],
               id_cbg: str_kia,
               id_qgi: str_tth)
              (UNION(id_kwy, id_nba, id_cin, id_fyh, id_rar, id_cxr, id_xcr).
               (str_bni$0 <= str_nzm| {} , str_cwo))
          )
          (
            rec
              (id_psb:
               (rec
                  (id_pku: str_plp,
                   id_beq: str_iev,
                   id_ymd:
                   ({ id_xrk, id_nav, id_aot, id_vqq, id_ekd, id_msh, id_xla,
                      id_zpj
                      |
                      str_wyi /<: str_gwd }
                      (str_npu)),
                   id_xum:
                   {str_irh, INT, str_jfn$0, 42, str_ogj$0, str_oet$0},
                   id_ldi: NAT1) |>>
                (left(BOOL))))
          )
        
        :=
        infix
          (father
             (PI
                (id_myr, id_egi, id_hsq, id_ksr, id_dnb, id_gbv, id_hrp,
                 id_vbj).
              (#(id_bko).(FALSE /: BOOL)|
               { id_rho, id_hjv, id_vmv, id_kdb, id_whu, id_nsz, id_xjb,
                 id_pac, id_pok
                 |
                 str_trp /<: str_lmn })
                (POW1(str_vhj$0))))
      OR
        ({ id_iqh, id_hvv, id_lcr, id_kvl, id_rtr, id_mlw, id_wml, id_bxx,
           id_lwk, id_pyk
           |
           !
             (id_auy, id_jpv, id_lde, id_shl, id_fhg, id_kia, id_aqi, id_heo,
              id_jmo).
           (str_uyh$0 <<: str_nso$0) =>
           #(id_qoi, id_wae, id_ehc, id_vsd, id_lpw).(str_yaa > INTEGER) } |>
         [str_jfa, str_sdy, NATURAL, str_eex$0, str_rlf])
      THEN
        id_uym
          ([bool
              (!
                 (id_oof, id_zor, id_css, id_aef, id_hsy, id_wzb, id_lrf,
                  id_fkq, id_kzj, id_zmo).
               (str_eff /<<: str_tdo)),
            { id_njr, id_sdo, id_xei, id_tih, id_ibi, id_jjm, id_cgz, id_bkk
              |
              !(id_pue).(INT : str_xvt or not(NAT /<: STRING)) },
            str_gja],
           bool
             (struct(id_hlv: bool(MININT < NAT1)) <:
              bin(closure1(bool(BOOL <: str_ock$0)))))
      OR
        struct
          (id_hac: str_gjx$0,
           id_unk: str_eaf$0,
           id_xbt:
           struct
             (id_pqn:
              {%(id_ssh, id_ejz, id_oto, id_apc, id_usy, id_rsy, id_oxy).
               (INTEGER >= str_urs$0| left(str_agk'id_eby))},
              id_ofs: (str_ofg +-> INT),
              id_ziq: str_jty,
              id_kjs: str_zwg,
              id_zzp: str_oey$0,
              id_mbu: str_xiy$0),
           id_kxs: str_qyi,
           id_phh: STRING,
           id_ohz: MAXINT)
      THEN
        id_bbk, id_rus, id_ahe, id_qzk, id_uuy, id_cvo, id_clm, id_iur,
        id_bcj, id_anf :(
          bool(MAXINT >= INTEGER) /=
          bool
            (!(id_vjh, id_orn, id_rdx, id_qlq, id_oux).
             (str_rtm$0 /: str_gxg$0 => str_jgt$0 <= str_gyy)) or
          (str_bun$0 > NAT1 or str_fmd >= str_hgu$0)
        )
      ELSE
        id_xqv, id_dyt
        <--
        id_sml
          ({rec
              (id_wnp: NAT,
               id_yjm: str_jyb,
               id_evq: (str_nwf$0 , (str_knq(str_ukd$0))),
               id_djg: bool(str_snu /: str_auk$0),
               id_ikm: str_tcb,
               id_jua: str_hye,
               id_fhp:
               [str_cwo$0, FALSE, {str_ryw, str_zdi$0, str_rpz, FALSE}])},
           rec
             (id_gha: BOOL,
              id_jbr:
              bool
                (not
                   (#
                      (id_ikh, id_cat, id_jyf, id_tue, id_xef, id_xdo,
                       id_bhj, id_zxv, id_qiv).
                    (str_irl /<<: INTEGER &
                     #
                       (id_snl, id_siq, id_iwd, id_aht, id_koy, id_emr,
                        id_jpr, id_bar, id_mol, id_axd).
                     (str_oge /<: str_ken$0)))),
              id_puc: bool({} /<<: MAXINT),
              id_grs: "str_rcl"))
      END
    END
  ELSIF
    {rec
       (id_xpi: BOOL,
        id_xoi: INTEGER,
        id_zeh: (card("str_vpw")),
        id_nxc: str_ogs,
        id_tek: str_ecs,
        id_caf: MAXINT,
        id_jsh: str_rnp$0),
     str_vjo$0} :
    { id_nom, id_efo, id_qtz, id_nyd
      |
      !(id_oag, id_ojk, id_rek, id_coy, id_zjk).
      (!
         (id_acu, id_fux, id_egy, id_evr, id_fml, id_tux, id_rax, id_xly,
          id_zvo).
       (TRUE <<: str_cup$0)) }
  THEN
    IF
      (str_udp$0 /= str_uqe$0 or #(id_bao, id_cmy).(str_nky : str_sll$0)) <=>
      #(id_qqy, id_kjr, id_esy, id_kuq, id_ejo, id_zmg).
      (str_qjx'id_ezf <: str_bti$0)
    THEN
      CASE
        { id_kzw, id_qyv
          |
          !(id_gva, id_osa, id_mko, id_xxt, id_uxi, id_mke, id_yiy, id_dkf).
          (#(id_zeo, id_jhd, id_rkv).
           (#(id_ojc, id_rlo, id_qih, id_vss).(not({} /<: str_zlb$0)))) } -
        (str_qxi , str_fce$0)
        OF
        EITHER
          INTER(id_zge, id_gvn, id_maf, id_wqv, id_tox).
          (str_qcw$0 <<: NAT|
           bool
             (#(id_fpz, id_shd, id_lkd, id_idd, id_jzu).
              (not(not(str_cxa /: str_dvq$0))))
             (struct(id_juw: str_pvk, id_rsl: "str_lxt", id_cgp: str_amo)))
        THEN
          id_uys, id_ksz :(
            not(str_ltb$0 >= str_qrf) <=>
            PI(id_xwz, id_qps, id_rag, id_rcq, id_lmd, id_dwy).
            (str_aql <<: INT| BOOL) <=
            prj2({ id_xiw | str_npr = str_hpq })
          )
        OR
          [struct(id_sbm: NAT1),
           (str_gqy(str_uhh)),
           {},
           str_tns$0,
           NATURAL1,
           (str_ydw$0[str_qur])'id_dwf,
           rec
             (id_exu: [],
              id_pmq: (rank(str_jnv)),
              id_jup: str_pan$0,
              id_hfg: str_rky$0),
           TRUE]
        THEN
          id_kpm'id_qtx
          :=
          %(id_tft, id_yjt, id_rgm, id_qfz, id_pax).
          (not(str_ppn /<<: str_zfo$0 <=> str_ncs$0 < str_fcl)|
           dom({str_zpi$0, (str_kuv$0 >< (str_vkp ** str_oms)), NATURAL}))
        OR
          (seq1
             (struct(id_ymx: (FIN1(str_aju$0)))
              ,
              (str_wql
                 (struct
                    (id_lai: str_klj,
                     id_lcd: 9,
                     id_oyf:
                     { id_zml, id_pgl, id_uax, id_dss, id_uvn
                       |
                       INTEGER /<<: str_rtv$0 },
                     id_xrt: str_tmf$0,
                     id_nul: TRUE,
                     id_car: str_wti$0,
                     id_car: str_zvr$0)))))
        THEN
          IF
            #(id_fho, id_igw, id_vav, id_qgb, id_qhe, id_fyd).
            (#(id_pwo, id_fdp, id_eat, id_ylv, id_mwh, id_zro).
             (#(id_xui, id_agm, id_vlk, id_xfc, id_gxp, id_qbt, id_iyy).
              (#(id_yxx, id_wqp, id_dcc, id_xif, id_lba, id_sbo, id_eyj).
               (#
                  (id_rbx, id_rpa, id_vgv, id_twk, id_hfo, id_ysh, id_ldx,
                   id_kuc).
                (#(id_tgj).
                 (!
                    (id_twa, id_dhl, id_atk, id_nza, id_skd, id_jyz, id_zcd,
                     id_vbr).
                  (str_off /<<: INTEGER)))))))
          THEN
            id_iaf
            :=
            INTER(id_pqi, id_mqo, id_tpr, id_law, id_deq, id_ooi, id_sss).
            (str_pkk <<: str_lke$0| iseq(str_oir$0)) +->
            [STRING,
             [BOOL, NAT, MAXINT, str_bto, str_gzs],
             (str_lev
                (rec
                   (id_jtm: str_ieu,
                    id_iud: STRING,
                    id_osh: str_aso$0,
                    id_rts: str_yjg)))]
          ELSIF
            #
              (id_qgn, id_kii, id_gnr, id_jxv, id_ihh, id_dfd, id_suw,
               id_yhh, id_nlo).
            (not
               (str_yxy /<<: str_hxj =>
                #
                  (id_fdx, id_lfs, id_sqt, id_hau, id_pdp, id_mop, id_fqp,
                   id_azo).
                (#
                   (id_rdt, id_cad, id_ghu, id_itp, id_jxm, id_vuy, id_usg,
                    id_yvl, id_oph, id_esp).
                 (str_ydj <:
                  [MAXINT, str_wte, str_cdz, str_vou$0, str_fsr, str_xcr,
                   BOOL, str_yat$0]))))
          THEN
            id_kfo, id_qfv, id_ztg, id_hol, id_rih
            ::
            bool
              (str_xst /=
               rec
                 (id_uhp: (INTEGER(str_pix)),
                  id_imo: str_mxc,
                  id_lvo: [NAT1, str_mti$0, str_gex$0, str_dez, str_nvr],
                  id_txo: str_uqm) +->
               NATURAL1'id_klz)
          ELSIF
            !
              (id_cyq, id_eww, id_bfc, id_zlj, id_lut, id_ocz, id_yxk,
               id_eeo, id_ucf).
            ((MININT = str_xwf or [] : str_pwz$0) =>
             not
               (not
                  (!(id_tdq, id_nse, id_eun, id_ial, id_vmk, id_mms, id_tah).
                   (str_jbb : NAT1) or
                   str_dig$0 <<: str_hbi)))
          THEN
            id_ylu, id_bwh, id_vyc, id_twr, id_hdv
            :=
            [UNION
               (id_qat, id_icw, id_xat, id_zoa, id_bbn, id_roh, id_lao,
                id_ctb).
             (str_qbr$0 = {} or
              !(id_lwr, id_fkt, id_ago, id_pxb).(str_kqh$0 <<: str_jkv$0)|
              INTER(id_nro, id_ufv).
              (not(str_vjg$0 <<: str_sih$0)| MAXINT , STRING))]
          ELSIF
            not
              (#(id_tno, id_vzv, id_rhl).
               (struct
                  (id_byc: ((str_cwt$0 , str_lth) , str_yie$0)'id_mmt,
                   id_lmu: str_wha) =
                str_oig))
          THEN
            id_fhi, id_bsq, id_ciu, id_tml, id_nck, id_fua, id_sgb, id_hfs,
            id_brn
            <--
            id_cyk
              ((-
                  (struct
                     (id_xfx: INTEGER,
                      id_nao: (str_rxt |-> str_kfa),
                      id_opc:
                      {str_jfo$0, "str_qxv", str_sld, str_ljx, str_vvv$0})
                     'id_kaa +->>
                   struct(id_cyt: str_ize, id_lgl: str_bqv))),
               struct
                 (id_mos:
                  ({ id_dmd, id_nve, id_hdp, id_yjt, id_tfs, id_vbg, id_hsl,
                     id_ezp, id_mmm, id_zct
                     |
                     !
                       (id_fli, id_ghf, id_hhj, id_tgf, id_ytm, id_cfk,
                        id_cps, id_cdx, id_klt, id_wnj, id_zho).
                     (str_zqq : STRING) } **
                   STRING),
                  id_fmx: [str_kbm],
                  id_qsq:
                  [NAT1,
                   struct
                     (id_vuu: str_qjf,
                      id_sll: str_lxy,
                      id_cul: NAT,
                      id_fhi: str_lcu,
                      id_ttb: str_uqq$0,
                      id_czv: MAXINT,
                      id_ula: str_ztx,
                      id_rex: str_rsr),
                   INT,
                   NAT,
                   [],
                   str_jgx$0]),
               ([PI(id_ibf).
                 (#(id_vct, id_qfq, id_azz, id_wqe, id_rfk).
                  (str_pag$0 /: MININT) or
                  2 >= NATURAL1|
                  str_lpq$0),
                 str_prx,
                 STRING,
                 { id_tms, id_bep, id_fev, id_hdg, id_jjl, id_ivj, id_soe,
                   id_zzi, id_rbt
                   |
                   INT > str_ysx$0 },
                 str_yiq$0]
                  (str_rsm$0)),
               bool
                 (#(id_adm, id_txo, id_gpr).
                  (!
                     (id_ref, id_ghr, id_otl, id_qub, id_use, id_aqz, id_tai,
                      id_tsw, id_lss).
                   (str_swv$0 >->> str_csd <<: NATURAL |> str_iqe)))
                 'id_xtq,
               { id_coz, id_czv, id_ivn, id_get, id_gvj, id_idy, id_gac
                 |
                 not
                   (#(id_qie, id_kcx, id_akn).
                    (not
                       (str_rbr = MAXINT =>
                        !(id_dvf, id_vnb, id_bjj).
                        (str_bhy$0 = str_ngp$0 or str_qhd$0 <: str_aob$0)))) },
               [struct
                  (id_rtw: str_nnm$0,
                   id_lda: str_iuu$0,
                   id_thl: str_gjq,
                   id_jik: (mirror(NATURAL1)),
                   id_eqw:
                   {str_dyi$0,
                    str_oaz,
                    rec(id_yeq: str_lke$0, id_poq: str_ruk, id_wcs: []),
                    str_rfw,
                    {str_rcm, str_cpd, str_pyf$0, str_bqo$0, str_vrg$0,
                     str_dlr$0},
                    str_qoo$0,
                    str_yur}),
                PI(id_plm, id_awd, id_amn, id_vzc, id_ljj, id_qzy, id_mib).
                (str_nmu /: str_cqh| TRUE),
                9],
               struct
                 (id_cjt:
                  (bool
                     (#
                        (id_rcf, id_ljq, id_amv, id_tnl, id_bid, id_uzh,
                         id_vyu, id_kbb, id_mzi, id_usc).
                      (#(id_vbu, id_chd).(INT /= TRUE)))
                     ({ id_dru, id_gms, id_sec, id_ynn, id_fcy, id_tqa,
                        id_yax, id_jow, id_wyu
                        |
                        str_hrc$0 : str_cuy$0 } **
                      str_wbs$0))))
          ELSIF
            not
              ({str_onc$0, str_tqd$0, str_aeo, MAXINT, (str_imq$0 - str_wrf)}
               ,
               str_ukk <<:
               struct(id_vtf: NAT, id_yti: (son(STRING))))
          THEN
            id_uxq, id_knw, id_tei, id_xzq, id_tcf, id_qcd, id_mvp, id_tvt,
            id_apy :(
              str_zwm$0'id_ale'id_rzz /:
              struct
                (id_bed: INTEGER,
                 id_fqa: str_dvo$0,
                 id_jhu: MININT,
                 id_fgu: NAT,
                 id_jrj: (str_rty , str_mni),
                 id_tyo:
                 INTER(id_ydj, id_mgh, id_ume, id_one, id_rki, id_kqf).
                 (str_yec /<<: INT| MININT)
                   'id_rgn)
            )
          ELSIF
            rec
              (id_qxq: str_yze$0,
               id_pyb: [NAT1, FALSE, str_ucn, NATURAL, str_vqy$0]'id_znv,
               id_nqh: str_ysz$0,
               id_wud: str_asn,
               id_hkn: (dom(str_quz)),
               id_zwq: MININT,
               id_oms: str_zly,
               id_vsr: str_gca) /<<:
            [str_ttw,
             ([] <-> MININT),
             str_xge,
             str_wst$0,
             TRUE,
             BOOL,
             str_efv$0]
          THEN
            id_xra'id_dze
            :=
            { id_ehd, id_awk, id_ypz, id_fvw
              |
              #(id_apf, id_wjv, id_alv).
              ({ id_rcx, id_ojr, id_wpg, id_dvt, id_oaq, id_yyv, id_vgl,
                 id_ulr, id_jqh, id_zbp, id_uem
                 |
                 str_exg = INT } /<<:
               POW1
                 ({ id_xjg, id_loa, id_iif, id_zwm, id_ehr, id_dfs, id_ytk,
                    id_bls, id_ons, id_per
                    |
                    #
                      (id_dzx, id_dlv, id_dqp, id_sun, id_dab, id_ixt,
                       id_ocg, id_quf, id_lho, id_nbg).
                    (str_eip$0 >= str_sfl$0) })) }
          END
        OR
          ((str_fit$0 -->> str_owl)'id_pzt
           |->
           bool
             (!(id_cwy, id_qds, id_nzq, id_uat, id_emw, id_fgv).
              (str_rrx$0 = TRUE <=> str_zvl$0 <<: MAXINT)))
        THEN
          id_wot, id_xwx, id_wfu
          ::
          UNION(id_kwd, id_etc).
          (not(str_xyr$0 /: str_kgr or FALSE >= str_yvx)|
           bool
             (#
                (id_xyj, id_ckm, id_qzl, id_mak, id_hyr, id_xju, id_oti,
                 id_puu, id_one, id_kdy, id_ail).
              (INTEGER >= INTEGER)))
            (str_fhf)
        OR
          ((([str_vjx, str_eeb, str_ehl$0, NATURAL, str_det, str_mjz$0, {},
              MAXINT]
               (str_uzh$0))
              (str_krn$0'id_mgb))
           |->
           %
             (id_whu, id_edl, id_ydx, id_dec, id_qhx, id_ekv, id_yfv, id_wwg,
              id_cyp).
           (str_zbi$0 = []| {str_yjg, str_ptd, str_wnh}))
        THEN
          id_npc, id_ore, id_buf, id_wgi, id_xqv, id_wmv, id_kfk
          ::
          UNION(id_twi, id_msf, id_lgg).
          (#(id_cgf, id_qga, id_ele, id_ahe, id_msj, id_fbx, id_zwn).
           (#(id_vae, id_ywp, id_rsk, id_apl).
            (#
               (id_mqa, id_ecw, id_vmo, id_jjd, id_sne, id_ofd, id_ylf,
                id_flw, id_hfw, id_mwh).
             (#(id_ivb, id_gby).(str_woi <<: INTEGER))))|
           SIGMA(id_bxi, id_vkm, id_uol, id_prj, id_cdf, id_tum, id_sfg).
           (str_yqa$0 <: str_pfz$0|
            struct
              (id_twu: FALSE,
               id_wzx: {},
               id_njn: str_ixl,
               id_vpp: str_mfr$0,
               id_efr: str_hzc$0,
               id_kce: str_som$0,
               id_uop: str_kpt$0,
               id_fxk: str_luj)))
        OR
          { id_ylc, id_bhb, id_yuj, id_viv, id_bpf, id_gyy, id_ntd, id_wuq,
            id_khd, id_cja, id_ley
            |
            !(id_avt, id_xqh, id_wsg, id_zie, id_xqe, id_krh, id_eci, id_pud).
            (not(not(str_uvt <= BOOL) & str_cbl$0 /<<: INTEGER)) <=>
            #(id_ail, id_rtr, id_rrp, id_nix, id_xia).(str_zhe < INT) }
        THEN
          id_qmt, id_mzq, id_izv, id_nyn, id_hed, id_lsv, id_krc, id_qqy,
          id_pvl, id_fvi
          <--
          id_qox
            (rec
               (id_zad:
                UNION(id_pol, id_fvz, id_uym).
                (str_max$0 <: str_buu| str_vxy),
                id_lei:
                (NATURAL
                   (struct
                      (id_cxh: ((str_kne$0 || str_air$0)),
                       id_ueq: str_cgh$0,
                       id_kje: str_ldn,
                       id_wnn: str_oqp,
                       id_ryw: str_ttg$0,
                       id_xhj: NATURAL1,
                       id_prk: str_shr$0,
                       id_bpu:
                       struct
                         (id_vbw: str_hxv,
                          id_jwa: str_mkp,
                          id_bhc: NATURAL1,
                          id_rqj: str_hxe,
                          id_uvg: str_zep$0,
                          id_mxz: BOOL,
                          id_wba: str_bez$0,
                          id_jtg: str_hsu))))),
             rec
               (id_hwx:
                { id_tjm, id_edn, id_pcq, id_prd, id_uef, id_tzn
                  |
                  !(id_zcg).(str_gaw$0 <<: str_yuh) },
                id_cmw: NATURAL,
                id_mml: str_pxv$0,
                id_kao:
                PI(id_pso).
                (!
                   (id_tcq, id_hpe, id_xeb, id_gvt, id_kiy, id_yag, id_tqa,
                    id_lha, id_zqq).
                 (#(id_tuk, id_jnu, id_xdj, id_hzd, id_dlk).
                  (#(id_icg, id_uwa, id_ulc, id_ouw, id_drq, id_czx).
                   (str_kru$0 /<<: str_roh)))|
                 str_fec$0)),
             UNION(id_yxi, id_hue).
             (not
                (#(id_ctl, id_oaw, id_vyb, id_mtt, id_mgr, id_jvx, id_xgs).
                 (!(id_myj, id_khl, id_mru, id_rrv, id_rfm, id_bdt, id_cfi).
                  (#
                     (id_wip, id_wyp, id_tzt, id_bkl, id_etq, id_ucr, id_lrb,
                      id_dqb, id_riv, id_vsw, id_fav).
                   (INT : str_umh$0))))|
              rec
                (id_ruc: MAXINT,
                 id_ltz: (prj1(str_iob$0)),
                 id_rie: MININT,
                 id_jlb: str_cwh$0,
                 id_ymr: STRING)),
             (sizet
                (struct
                   (id_gbv:
                    (struct
                       (id_xbr: INT,
                        id_fwf: str_hht,
                        id_tyj: str_bed$0,
                        id_rfl: NATURAL,
                        id_ucl: STRING,
                        id_tfp: str_tvn$0,
                        id_ttz: struct(id_ahu: TRUE, id_rya: str_qpo),
                        id_oup: str_voq) ><
                     (left(str_xxl$0))),
                    id_jrj: NAT'id_nov,
                    id_jpe: str_ocw$0,
                    id_fly: str_kwr$0))),
             ([str_dkg$0,
               INTER
                 (id_wif, id_tdp, id_ldy, id_rwq, id_aiu, id_sie, id_jfx,
                  id_efy, id_dja, id_ahc).
               (str_cbr$0 /: str_yhf| str_wvc$0([])),
               {{{}, str_pch}},
               str_gzj,
               str_kmp$0,
               str_aod$0,
               rec
                 (id_zcl: FALSE,
                  id_eqc: STRING,
                  id_icc: str_eem$0,
                  id_nqh: str_vkp,
                  id_mxn: str_bmz$0,
                  id_qmu: str_pvz,
                  id_mnx: str_cgj)]
                (str_vvt$0)))
        END
      END ||
      id_myc, id_lnc, id_hcd, id_uff, id_qup, id_gqz, id_gzn, id_bjg
      <--
      id_ypk
        (UNION
           (id_vpd, id_giv, id_ukn, id_ryc, id_xnz, id_tgg, id_wgu, id_qhn).
         (#(id_ffu, id_tro, id_kdy, id_zxk, id_tll, id_kua, id_klm, id_yhz).
          (!
             (id_qpz, id_mxn, id_xkb, id_bsm, id_apq, id_wyx, id_gcz, id_jsy,
              id_hqj, id_xyj, id_iiy).
           (#(id_pry, id_tpb, id_avn, id_xzo, id_rfr).
            (#
               (id_phb, id_xvw, id_aht, id_tdz, id_jye, id_nro, id_xqj,
                id_kiz, id_mqd, id_zel).
             (#(id_spu, id_ruc, id_skm, id_xuw, id_rcg, id_ots, id_tol).
              (STRING <<: str_cxq$0 or str_dcs > MININT)))))|
          str_lwh$0)) ||
      id_lls, id_rcr, id_jqq :(
        struct
          (id_kfl: MAXINT,
           id_aqx: str_nqv$0,
           id_hpu:
           bool
             (!(id_zcl, id_pfc, id_mnk).
              (#
                 (id_pix, id_erf, id_axx, id_yeo, id_ulm, id_oqu, id_nbb,
                  id_luf, id_xjd, id_pay).
               (str_zbc$0 /<<: str_jwb & STRING > NAT1) &
               str_kxi$0 <= str_phz$0)),
           id_rxt: str_nxo) <=
        str_kfc$0
      )
    ELSE
      id_agc
      <--
      id_tlc
        (bool
           (not
              (#(id_uwt, id_ylp, id_fab, id_klw).
               (!
                  (id_rvs, id_eps, id_msh, id_njr, id_ukg, id_lwh, id_frp,
                   id_was, id_qrs).
                (not(str_fiz$0 <: NAT => str_vld > BOOL)))))
           'id_efa,
         %
           (id_lwv, id_lzb, id_iqr, id_hao, id_xyz, id_adj, id_wrn, id_gnk,
            id_qfp, id_yvv, id_ypw).
         ([str_wng$0, str_fgj$0, ([] , FALSE), str_kxg, str_tyc] <<:
          { id_blj, id_sbp, id_sbo, id_xyp
            |
            str_lev : str_tcw & NAT <= str_unc$0 }|
          str_fxp$0'id_nap))
    END
  ELSIF
    not
      (not
         (!(id_lkz, id_stv, id_iso, id_xok, id_prg, id_bgl, id_rbx, id_cdl).
          (#(id_iue, id_fnz, id_fex, id_lmo).
           (str_ubh <: str_att$0 &
            #(id_wyh, id_vpb, id_teg, id_qhg).
            (#(id_xki, id_fwx, id_nui, id_bqv).(NAT1 /<<: str_agz$0))))))
  THEN
    id_ciq 
      (
        {struct
           (id_rhc: str_yjs$0,
            id_xam: NATURAL1,
            id_ozg: MAXINT,
            id_pnr: str_mnh,
            id_zwr: [],
            id_jng: str_zvy,
            id_rvz: FALSE,
            id_rcg: str_lro$0),
         str_fdt'id_eye,
         {(str_uju , BOOL), "str_xbg", (sons(str_drz$0)), ([] /\ str_fbl)}}
      )
    
    :=
    right
      ([{ id_jnj, id_bdc, id_jmf, id_bfy, id_dwk, id_mdf, id_qdd, id_yqi,
          id_zfx
          |
          not(not(str_nww <<: str_hnn)) }
          'id_reo])
  ELSIF
    PI
      (id_gqq, id_wnc, id_lnh, id_xbh, id_inc, id_uxe, id_ilm, id_jhb, id_frz).
    ({str_rak, str_txa$0} /<:
     rec
       (id_ihi: str_eww$0,
        id_zmb: [],
        id_tbc: NATURAL1,
        id_qyt: str_tvh,
        id_zer: str_pzr$0,
        id_fyh: NAT)|
     str_obp) <=
    [NAT1,
     %(id_flr, id_hhc, id_onv, id_hvd). (NATURAL1 /<<: BOOL| "str_pqk"),
     str_kdm$0,
     str_esk$0]
  THEN
    id_wwr 
      (
        { id_npk, id_lfn, id_kyw, id_bhp, id_smd, id_gja, id_okv, id_ktu,
          id_dov
          |
          NATURAL1 <<: str_siq or str_qfk$0 > str_oaa$0 } mod
        (str_qoh /|\ (postfix(str_gzq[MAXINT , str_uzq$0])))
      )
      (
        [INTER(id_pxy, id_kvj, id_ibc, id_wat, id_zyx).
         ({} /<<: str_iit| NATURAL),
         (str_zjb(bool(MAXINT <<: str_yny)'id_hve)),
         ((mirror(BOOL)) |-> str_mkj)]
      )
      (
        struct
          (id_yud: ((FALSE + NATURAL) /\ TRUE),
           id_zti: str_wns$0,
           id_bzy: bool(not(!(id_giz, id_xbv).(str_mfc <= str_pfe))))
      )
      (
        { id_gmf
          |
          !(id_zhv, id_zcd, id_zcj, id_jcd, id_oiq, id_usb).
          (FALSE , NATURAL1 <= bool([] /<<: str_qfk$0)) }
          'id_eqz
          'id_vui
      )
      (
        [(str_qsn$0 >< (MININT -> NATURAL)),
         rec
           (id_osr: (mirror([])),
            id_xfj: str_rls$0,
            id_ifu:
            rec
              (id_wsk: [NATURAL],
               id_kot: NATURAL,
               id_ffp: str_syv,
               id_yqt: str_xby$0),
            id_jzw: str_dzl$0,
            id_iqx: 6)]
      )
      (
        bool
          (#
             (id_cur, id_nfv, id_tdp, id_jji, id_jws, id_ewe, id_exg, id_ail,
              id_obm).
           (!
              (id_rsn, id_ljb, id_qwb, id_ntr, id_ila, id_mje, id_day,
               id_pis, id_mux).
            (not(str_yvi$0 <= str_mqt$0)) <=>
            #
              (id_xqg, id_jys, id_qas, id_hfh, id_zee, id_pml, id_uxi,
               id_boz, id_wmx).
            (BOOL = str_gnf$0 or str_suq$0 : str_pdu$0)))
      )
      (
        {%(id_ufq, id_ofo, id_rak, id_ftv, id_phi).
         (str_awd$0 >= str_xqj => str_jwe$0 > str_xsp$0|
          {str_npj,
           str_akb,
           str_oki,
           str_mgt,
           (MAXINT , str_dgg$0),
           str_mec$0,
           INT}),
         { id_ffi, id_cxt, id_wef, id_kdf, id_nes, id_ohu
           |
           not(str_wfx = str_mjz$0) },
         str_fql,
         FALSE,
         str_ojr}
      )
      (
        %(id_rop, id_wod, id_bii, id_tak, id_cwi, id_qxh).
        (str_ikx /<: str_vcd| str_wnp) /
        rec
          (id_mmo: (prj1((str_gba$0(str_nxz)) |-> (str_kpy$0(INT)))),
           id_mms: str_zyq)
      )
      (
        SIGMA(id_svc, id_rei, id_yza, id_tcx, id_uyq).
        (not
           (#(id_tly).
            (#(id_tgb, id_vyg).(not("str_yuq" < []) => str_bmv = [])))|
         [str_pon, str_stv$0, str_ziy, str_nqa])
      )
      (
        (str_bng
         |->
         [str_fbk,
          NAT1,
          struct
            (id_htx: [str_uzj, (str_xku , str_lhi$0), str_jce$0],
             id_exg: str_sjw,
             id_vdv: str_xkf$0,
             id_gjx: {})]
           'id_ebj)
          'id_juj
      )
    
    :=
    {{str_gdk, str_brh$0, str_vcb, str_fla, str_lxu$0, {}, str_ezg$0},
     struct
       (id_fxp: INTEGER,
        id_dxr:
        UNION
          (id_ins, id_mjg, id_rpn, id_hiu, id_ovv, id_mij, id_ykc, id_osi,
           id_yap, id_dmr).
        (str_jzq <= str_jiw| bool(str_uik <= str_kqz)),
        id_bsh: MININT'id_jop)}
      'id_khk
  ELSIF
    #(id_uau, id_lgi, id_gqp).
    (struct
       (id_vcy: MININT,
        id_bkh: rec(id_wdm: bool(str_xlj$0 <= NAT1 or str_krj$0 /<: str_ahi)),
        id_khz: str_vgp$0,
        id_xyo: bool(str_pba > NATURAL),
        id_thq: INTEGER,
        id_hox: str_edn) <:
     str_exw)
  THEN
    id_gbm
    :=
    bool
      (#(id_wpm, id_vsc, id_ipg, id_lyw, id_ycb, id_hsw, id_klp).
       (not(str_esv$0 <: STRING) =>
        NATURAL <=
        str_afv
        |->
        { id_hvu, id_nog, id_ynn, id_yxl, id_his, id_mru, id_rgb, id_stz,
          id_brb, id_ums, id_mxt
          |
          str_dxq /= str_udf }))
  END;

  id_hgh, id_jtl, id_emr, id_aar, id_ewy, id_iwj, id_uhf, id_qwu, id_lns,
  id_rsh
  <--
  id_mty
    (id_rwt, id_zqb, id_dyb, id_mps, id_hjl, id_kez, id_rvl, id_ngu, id_mbt)
  =
  ANY
    id_wqb, id_ndn, id_xpf, id_lti
  WHERE
    not
      (!
         (id_bdd, id_ggp, id_clr, id_jxp, id_ast, id_tkf, id_jxl, id_lie,
          id_tux, id_lca, id_niz).
       (not
          (!(id_hkp, id_gak, id_xvs, id_ntf, id_tsc, id_gxa, id_qsy, id_qax).
           (str_vzw /<<: str_rxt$0))) =>
       #(id_hrb, id_chd, id_spl, id_zgz, id_sol, id_dyw, id_jfh, id_dax).
       (not(str_shu < str_fqk)))
  THEN
    CASE
      iterate
        ([MAXINT,
          TRUE,
          str_teg$0,
          INT,
          3,
          NAT,
          INTEGER,
          ([rec
              (id_iyw: (BOOL , [str_vyx, INTEGER, MAXINT, INT]),
               id_sby: MININT)]
           |->
           str_rkl)])
      OF
      EITHER
        rec
          (id_rur:
           %(id_hsg).
           (!(id_ydp, id_psk, id_frn, id_ixa, id_jij, id_dcf, id_ddo, id_cwb).
            ((str_epa > INT => {} <= TRUE) or not(str_odi /<<: str_gos))|
            NATURAL),
           id_cxs: (str_ppm -> str_pky$0))
      THEN
        id_thk :(
          #(id_nye, id_adz, id_hno, id_lzw, id_lhq, id_mxs).
          (not(MININT /<<: str_ecr$0) =>
           [NATURAL, str_ega, str_mbs, str_ycb$0] |>> str_stl$0 < str_key$0)
        )
      OR
        {[str_dxo,
          INT,
          [],
          str_bji$0,
          { id_bok, id_umt, id_idq, id_cil, id_brj, id_tfu
            |
            str_rky$0 >= str_kec$0 },
          str_nqv,
          str_txq],
         ((str_gpg |-> (str_jeq$0 , bool(MININT /<<: str_ilo))) - str_aim$0),
         STRING}
      THEN
        skip
      OR
        (UNION
           (id_hxw, id_bej, id_nlp, id_bps, id_xdt, id_bxz, id_bfo, id_tjz,
            id_nfj, id_rik).
         (str_bab$0 /: str_wzv|
          (tree
             ([str_zec, str_puz, str_zzd$0, str_nfk$0, str_wuk, {}, str_ekn]))
            (MAXINT)) -->
         (SIGMA(id_bcj, id_lrf, id_xoy). (str_apk /<: []| str_trh$0)
          ,
          str_bvi$0))
      THEN
        PRE
          !(id_vao, id_wmw, id_bdl, id_sfr, id_ivl, id_rue).
          (str_mkh /= str_mxm or
           [{ id_waf, id_xqs, id_pdo, id_nvc, id_xkl, id_sbt | STRING <<: INT },
            MAXINT,
            str_xho$0] /<:
           rec
             (id_ydu: str_iou$0,
              id_fey: str_gcy,
              id_jzi:
              struct
                (id_kqw: str_nvq$0,
                 id_zfa: TRUE,
                 id_koa: STRING,
                 id_oho: str_dvb$0,
                 id_gbr: NAT1,
                 id_hos: str_mmy,
                 id_crq: str_wso,
                 id_ucx: str_ihv$0)))
        THEN
          IF
            INT >
            pred
              (bool(MININT < str_inz$0 => str_atu /<: str_pzu)
               ,
               bool(TRUE <<: str_zzg$0))
          THEN
            id_rvq
            :=
            (MININT +-> str_geu)'id_hxc
              ({ id_uhz, id_gwq, id_xjc, id_ehm, id_ivs, id_whb, id_rhv
                 |
                 !(id_vrp, id_csz, id_fgi, id_iml, id_fvs, id_lzx).
                 (str_ami'id_ajz > MAXINT) })
          ELSIF
            !
              (id_aza, id_yhg, id_cmv, id_xqu, id_pvb, id_gwh, id_gtw,
               id_kze, id_sld, id_pda, id_uml).
            (not
               (not([] >= NATURAL1) <=>
                !(id_ilt, id_odj, id_hpm, id_myt, id_ynl, id_qgk, id_ivz).
                (!(id_pmm, id_khv, id_rhx, id_mnf, id_ibq, id_bdh, id_mou).
                 (FALSE /: str_ijj$0 => str_nmn$0 /<<: str_smg$0))))
          THEN
            id_bsp, id_ltd, id_ahi, id_wwe, id_gma, id_epn :(
              not
                (#
                   (id_eap, id_gzd, id_jfo, id_cdp, id_rtp, id_tmt, id_qqy,
                    id_bva, id_xuo, id_ddp).
                 (not
                    (#(id_vxp, id_zdm, id_yxm, id_ucq, id_xvp, id_vke).
                     (#(id_byb).(str_vkg /: str_jhh) => NAT >= str_znr)) &
                  BOOL >= str_foi$0))
            )
          ELSIF
            str_ijl$0(str_fiv(str_syf)) :
            str_lyz$0(rec(id_xfv: (str_fgk , rec(id_yrg: {}))))
          THEN
            id_eaw
            :=
            [{ id_cth, id_ypm, id_tze
               |
               !(id_nax, id_sam, id_fon, id_zuk, id_unx, id_abf).
               (NAT1 /<: str_jyn$0) },
             STRING,
             { id_ypf
               |
               !
                 (id_kck, id_ryk, id_krw, id_beh, id_bod, id_tsi, id_ngs,
                  id_ias).
               (!(id_oln, id_jkx, id_ncf).(MAXINT /<<: str_pcg$0)) },
             str_fux,
             (str_dzl(str_nfh$0))]
          ELSIF
            str_moc$0 > INTEGER =>
            not(not(not(str_kaq$0'id_nah(str_gsr$0) < str_zep$0)))
          THEN
            id_nfp 
              (
                struct
                  (id_uak:
                   struct
                     (id_wlq:
                      bool
                        (!
                           (id_jma, id_fmw, id_gpv, id_kpb, id_gco, id_ebc,
                            id_puz, id_lyo, id_krb, id_noj).
                         (not(not(str_pxy$0 /<: INT)))))
                     'id_aya)
              )
              (
                PI(id_veq). (str_kcn >= str_iww$0| str_bou) **
                (bool
                   (!
                      (id_uin, id_wyg, id_xbd, id_top, id_rqv, id_rkb,
                       id_tpv, id_wde).
                    (str_eea /= str_cyn))
                 |->
                 [NAT, str_tci, {}])
              )
              (
                (str_mwr$0 +->
                 %(id_qdy, id_gfa, id_sgj, id_lyk, id_otc, id_kpj, id_pwn).
                 (!
                    (id_uel, id_ymt, id_wja, id_qcj, id_xni, id_kal, id_dya,
                     id_syg, id_tyx).
                  (not(str_awp$0 /: str_ftd$0))|
                  bool(STRING /= str_mlx$0 <=> str_cze /<: NATURAL)))
                  'id_zhp
              )
              (
                SIGMA(id_pkw, id_xls, id_aho, id_mtl).
                (not(not(str_hjd = succ(str_ckj)))|
                 { id_wtc, id_jri, id_fro, id_ysh, id_bke, id_xmb, id_rzb,
                   id_yhs
                   |
                   FALSE /<: str_cos or str_ids$0 >= str_wzr })
              )
              (
                %(id_fmu, id_gew).
                (!(id_azs, id_gla, id_anx, id_opc, id_mdc).
                 (!
                    (id_obl, id_dri, id_cfl, id_efc, id_pas, id_lxn, id_vsh,
                     id_epq).
                  (str_dsv$0 > STRING)) <=>
                 {} > str_srv$0|
                 SIGMA(id_too). ([] : str_ymw$0| left(str_sof$0)))
              )
              (
                bool
                  (str_auq , str_mxq <=
                   { id_dza, id_gsv, id_zzt
                     |
                     {} :
                     {{MININT, str_djd$0, str_xws$0, NAT, str_tyu$0,
                       str_dfw$0, str_ymz$0},
                      str_zsb} })
              )
              (
                {INT,
                 NAT1,
                 str_lwc,
                 str_jmx$0,
                 str_jhh$0,
                 str_cqm$0,
                 (POW
                    ({(str_bnk$0
                       |->
                       [str_lxk$0, BOOL, ({} - STRING), str_mpx])}))
                   'id_dnu}
              )
              (
                struct
                  (id_jpg: BOOL,
                   id_yyx: [],
                   id_uah: str_gjx$0,
                   id_loj:
                   {NATURAL1,
                    str_owc,
                    str_rff,
                    str_uej$0,
                    str_cjd,
                    MAXINT'id_zxg,
                    BOOL},
                   id_phb: (MININT(str_lol$0)),
                   id_kcy: str_xtp,
                   id_vmx: INT,
                   id_jqq:
                   bool
                     (!(id_ckq, id_dbq, id_hvf).
                      (!
                         (id_kvz, id_vga, id_xvn, id_ler, id_tuh, id_lsc,
                          id_sgu).
                       (str_jec : str_xhu$0))))
              )
              (
                (rec
                   (id_cno:
                    {INTER(id_qjs).
                     (#(id_spk, id_sue).(str_nev <<: str_agy$0)|
                      str_oub(str_ikd))},
                    id_awa: str_zer$0,
                    id_olp: NATURAL,
                    id_jwd: str_xsy$0)
                 ,
                 str_kaf)
                  'id_fbt
              )
              (
                prj1
                  (rec
                     (id_neu: str_nve$0,
                      id_agl: str_phj$0,
                      id_yar: str_kvp,
                      id_xmt:
                      rec
                        (id_mwh: str_axi,
                         id_dlu: str_uau$0,
                         id_yvk: BOOL,
                         id_hdx: str_gco,
                         id_bfb: str_udy$0),
                      id_yaq: str_mzp$0,
                      id_mjk: (conc(NAT1)),
                      id_wtv: str_szh)
                     ({{ id_cab, id_qma | str_ovu$0 /= str_bvv$0 },
                       str_fjb$0,
                       str_gqp,
                       []}))
              )
              (
                { id_wcl
                  |
                  NATURAL1 |-> str_lwg$0 <:
                  struct
                    (id_jps: str_dqd,
                     id_toi: str_mst$0,
                     id_vaf: str_weh,
                     id_cve:
                     struct
                       (id_axr: {},
                        id_znk: str_rqa$0,
                        id_htx:
                        SIGMA
                          (id_fpt, id_mxy, id_yjv, id_mda, id_yli, id_bgv,
                           id_dxa, id_xaq, id_oxw, id_cjg, id_mkb).
                        (str_bcb /= MININT| NAT),
                        id_nba: str_krp,
                        id_zwp: str_jgu$0),
                     id_vha:
                     rec
                       (id_akz: str_msb,
                        id_qki: str_ysp,
                        id_rvz: NAT,
                        id_zjr: str_gwv,
                        id_rju: str_bpo),
                     id_pgp: str_kek$0,
                     id_kij: str_qcp$0,
                     id_pre: MAXINT) }
              )
            
            :=
            [str_dzv'id_wlp,
             {(%(id_txj, id_orn, id_zfv, id_ths, id_khn, id_yau, id_axw).
               (INTEGER < str_cpz$0| str_jff$0)
                 (str_ifo$0))},
             [],
             rec
               (id_rib: str_zfr$0,
                id_ioo: (infix(str_ept$0)),
                id_yfu: str_oos,
                id_txi: str_zxk$0,
                id_duj: str_xqa$0,
                id_fgf: str_vhb$0),
             str_lzh$0]
          ELSIF
            rec
              (id_ctr: NAT,
               id_hfr: rec(id_tgf: (str_mtp |-> NAT), id_zoc: "str_dzp"))
              'id_akk /=
            [str_pei, str_hjd$0] , str_wux
          THEN
            id_rxt, id_yom, id_rey, id_nwx, id_klb, id_jpr, id_obj, id_ajj,
            id_eow
            :=
            {bool(str_xbr$0 = str_mqp <=> str_iei <<: str_wsz$0),
             (struct
                (id_guz: str_nko$0,
                 id_xbz: str_pak$0,
                 id_zlm: str_zcl,
                 id_aco: bool(str_ikd$0 >= "str_axb"),
                 id_icw: MININT,
                 id_yph: str_siv$0,
                 id_dsq: str_wlh)
                'id_cdh |>
              str_wjd$0)}
          ELSIF
            !(id_qdb, id_nlv).
            (#(id_jad, id_lls, id_izx, id_bia, id_wpn, id_arn, id_cqs).
             (str_rnz$0 /= NATURAL &
              (str_oiv >
               UNION
                 (id_gvi, id_zwk, id_qcw, id_xtc, id_esj, id_lkt, id_sqz,
                  id_gsq).
               (str_cbr /: str_uwl$0| str_tor$0) =>
               not(str_qlk$0 < str_vcs))))
          THEN
            id_zpo, id_xlb, id_ycu, id_dhq, id_mvv, id_cyr, id_uxi, id_lwq,
            id_luu, id_inl
            ::
            [FALSE,
             str_weg,
             str_drr,
             str_yup,
             ((str_qjz$0
               |->
               struct
                 (id_jyz:
                  SIGMA
                    (id_dbh, id_xpf, id_egy, id_ohy, id_spu, id_hsj, id_hgr,
                     id_acz, id_mhw, id_qcp).
                  (str_dov$0 >= str_ztu| str_swe$0),
                  id_ylc:
                  rec
                    (id_ihc: str_kqg,
                     id_gql: str_nal,
                     id_idd: str_jhs,
                     id_odq: STRING'id_okn,
                     id_org: NATURAL1),
                  id_xkr: str_ibw$0,
                  id_qzz: str_sbb$0))
                (str_idi))]
          ELSIF
            BOOL \/ rec(id_caq: (str_ugv(str_bda)), id_lup: str_zuz)'id_wsa /=
            str_hpq$0
              ({str_gze$0, str_shx$0, str_ios$0, str_xhg$0, FALSE, INT})
          THEN
            WHILE
              BOOL > FALSE or
              str_lzk$0 /<:
              bool
                (#
                   (id_mpw, id_cun, id_aji, id_wkk, id_bdv, id_dyr, id_acy,
                    id_bgm).
                 (not
                    (#
                       (id_vsn, id_oga, id_bhz, id_eyp, id_mxd, id_lyl, id_blf).
                     (NAT1 >= INT) =>
                     str_mux$0 : str_oku$0)))
            DO
              VAR
                id_fbf, id_kyr, id_hig, id_raj, id_lty, id_fkp
              IN
                id_dru 
                  (
                    SIGMA(id_vmn, id_qfg).
                    (INTER(id_bsq, id_gbs, id_xuj, id_njf, id_wqm, id_zty).
                     (#
                        (id_kdq, id_tsl, id_htf, id_gqo, id_wya, id_bix,
                         id_mds).
                      (NATURAL1 = str_nmf$0)|
                      str_tpb$0
                      |->
                      %
                        (id_hab, id_mgl, id_bve, id_cux, id_mcx, id_yhq,
                         id_pry, id_zqa, id_nwf).
                      (NATURAL1 < str_bod$0| NAT1)) >=
                     str_fvg|
                     rec
                       (id_elb: str_cxi,
                        id_lue: str_rdg,
                        id_bng: NATURAL,
                        id_jsl: str_mnf,
                        id_xwi: str_fhu,
                        id_njj: str_bpq))
                  )
                  (
                    {str_pyo,
                     NATURAL,
                     BOOL,
                     str_hvy,
                     str_fom,
                     struct
                       (id_kwe: FALSE,
                        id_cbj:
                        {SIGMA(id_itn, id_ogl, id_gxn, id_xyt).
                         (!(id_xwd, id_tsg, id_eek, id_ukw, id_egz, id_twe).
                          (str_fnh$0 /<: STRING)|
                          str_lrr$0),
                         str_jhd$0},
                        id_dki: str_gna$0,
                        id_dsl: NAT1,
                        id_cvm: str_gpy$0,
                        id_nln: ("str_dyi" |-> str_baz$0)),
                     { id_grq, id_loy | str_lml /: {} },
                     TRUE}
                  )
                  (
                    {{ id_qbn, id_wyk, id_daa, id_myn, id_ajc, id_sjm,
                       id_roz, id_vcj
                       |
                       !(id_pqq, id_mkh, id_kqs, id_elk).(str_rir >= str_brv) },
                     str_wmh$0,
                     { id_yps, id_cfh, id_hsz, id_xse, id_fkl, id_ogj, id_vqi
                       |
                       !
                         (id_mjl, id_bky, id_hox, id_jvm, id_mos, id_bmg,
                          id_igv).
                       (str_gjo$0 > MININT) },
                     SIGMA
                       (id_sft, id_kpr, id_yvr, id_rox, id_kkr, id_nzr,
                        id_vnl, id_ait, id_hhk).
                     (!
                        (id_qjw, id_ito, id_tzc, id_flv, id_jkp, id_sia,
                         id_ryn).
                      (str_aap /: INT)|
                      str_uny)}
                  )
                  (
                    { id_gdx, id_gmz, id_cio, id_byo, id_aes, id_fuy, id_gxc,
                      id_jdi, id_oti
                      |
                      str_wiq(MININT) <<:
                      UNION(id_jqj, id_pfz, id_msk, id_knn, id_dzz, id_kyw).
                      (FALSE <: str_mqd$0| right(max(MAXINT , str_omk))) }
                  )
                  (
                    rec
                      (id_zgi: {str_nxn$0, str_tsx$0},
                       id_rcx:
                       struct
                         (id_npk:
                          rec
                            (id_kwa: str_fzs$0,
                             id_ups: (str_lop |>> str_lam),
                             id_pwt: INTEGER,
                             id_gil: str_iyw$0,
                             id_apz: str_pup,
                             id_xfp: STRING,
                             id_yrc: ("str_dmf" , (mirror(str_myb))),
                             id_hwx: str_vsf)),
                       id_fgx: str_clr$0)
                  )
                
                :=
                bool
                  (#(id_jvc, id_nmp, id_lnr).
                   (!
                      (id_ddg, id_fpl, id_skf, id_rgi, id_lvy, id_gtf,
                       id_wrq, id_lgg, id_uzm).
                    (!
                       (id_yhk, id_ybn, id_oof, id_ael, id_feo, id_mlc,
                        id_eur, id_sqg).
                     (INT < str_lvw) or
                     !(id_iey).(str_sct$0 /= INTEGER)))) ;
                id_nfi, id_jdj, id_lyu, id_fyx
                <--
                id_euw
                  ({{ id_fry, id_kst, id_pil, id_ror, id_tgf
                      |
                      !
                        (id_vdq, id_fnu, id_erm, id_edr, id_eis, id_jun,
                         id_cxc, id_hfw, id_xyr, id_dur).
                      (#
                         (id_hxo, id_tks, id_tko, id_pwf, id_rki, id_dns,
                          id_gsg).
                       (str_xyt = str_mir)) },
                    (str_ghv$0'id_ymu(str_dfs))}
                     'id_npx,
                   ([{TRUE,
                      [STRING,
                       INT,
                       INTER
                         (id_evx, id_zsk, id_nxt, id_umm, id_wpo, id_bou,
                          id_bus, id_qrp, id_czk).
                       (NATURAL /<: str_rgl| str_clg$0),
                       str_qkf,
                       {},
                       str_hac$0,
                       NAT1],
                      str_jih$0,
                      (first(str_hnb$0)),
                      (str_mcl \|/ str_low),
                      str_cun,
                      str_nqy}]
                      (str_qxk)),
                   { id_coj, id_fhs
                     |
                     { id_jus, id_bbw, id_yji, id_fdk
                       |
                       str_ale /: str_hpi$0 &
                       {str_lzt$0, str_ymd, str_bey$0, str_pzn, TRUE,
                        str_xdn, MAXINT} <:
                       str_cqu } >=
                     struct
                       (id_ewl: str_udb$0,
                        id_bpi: NAT1,
                        id_yhd: STRING,
                        id_rbs: NATURAL,
                        id_err: str_hci,
                        id_seb: str_lzn$0) },
                   {SIGMA(id_ymh, id_zfn, id_esv).
                    (str_tuh$0 >=
                     PI(id_kfr, id_jhs, id_tgl).
                     (#
                        (id_qqy, id_dfq, id_asu, id_krp, id_rxp, id_ljm,
                         id_mmg, id_gba, id_mzr, id_vzb).
                      (str_qxt$0 <: str_hdm)|
                      str_oix$0)|
                     rec
                       (id_flk: FALSE,
                        id_uxb: str_cwj,
                        id_ehq: str_nqf,
                        id_oyw: str_cgp$0)),
                    [],
                    str_fso$0,
                    NATURAL1,
                    str_auv,
                    { id_ypj, id_zof, id_dmj, id_mmb, id_krp
                      |
                      str_gmg$0 = str_vgq }})
              END
            INVARIANT
              !
                (id_ekc, id_zrs, id_qeh, id_asz, id_pxh, id_gwq, id_pqg,
                 id_uch, id_nkp, id_vhu).
              (str_zhm <:
               bool(not(bool(str_jlk$0 <= {}) >= str_bkk |-> TRUE)))
            VARIANT
              %(id_jey, id_ruo, id_tai, id_jlz).
              (#
                 (id_smj, id_ovw, id_uua, id_scm, id_hkf, id_sjy, id_vgz,
                  id_vuq, id_dcc, id_mqs).
               (!(id_knk, id_wpe, id_hji, id_mps).(str_azz /<: str_wbe)) <=>
               str_mwj /<: str_knj$0|
               [NATURAL1'id_rns,
                str_rqs,
                str_grv,
                str_xcv,
                str_uxx,
                str_miz$0,
                {},
                str_ngj])
            END
          ELSE
            id_pmv, id_ijq, id_uae, id_tws, id_sjf, id_dsr, id_xkd, id_kdh,
            id_wgk, id_who :(
              not
                (#(id_kkn, id_zci, id_fgg).
                 (#(id_div, id_utr, id_umb, id_aie, id_euh, id_swf).
                  (!(id_zjp, id_lca, id_lgm, id_qcy).
                   (#(id_ppo, id_fpr, id_tzr, id_auk, id_jfz, id_hfv).
                    (!(id_ltc, id_hwn, id_slq, id_wue, id_irw, id_dei).
                     (#(id_qfs).(str_xym$0 /<: STRING)))))))
            )
          END
        END
      ELSE
        id_xtt'id_uux
        :=
        bool
          (!
             (id_mdo, id_tdo, id_brc, id_azo, id_exo, id_sir, id_amh, id_rev,
              id_edn, id_aih).
           (#(id_dxt, id_scl, id_yyz).(str_lbr$0 <<: str_lny)) or
           (#(id_svu, id_ide, id_vbh, id_uqq, id_ibu, id_gne).(STRING /= NAT) or
            str_xxp$0 <<: str_xdc$0))
          'id_okp
      END
    END
  END;

  id_bxz, id_cpq, id_mup
  <--
  id_bce (id_zmp, id_dny, id_eju, id_mpt, id_odn, id_fjt, id_hik)
  =
  WHILE
    rec
      (id_rvj: str_nlj$0,
       id_xph: str_pca,
       id_fxl: str_yay,
       id_izm: INTEGER,
       id_nwi: str_hne$0,
       id_xhe: MININT) :
    bool
      (not
         (not
            (!
               (id_hok, id_dsg, id_iwo, id_dvz, id_zhy, id_yyr, id_tod,
                id_xzw, id_ycf, id_ust).
             (not(NATURAL /<<: str_qmc$0)))))
  DO
    WHILE
      str_nqu /:
      bool
        (#
           (id_bwo, id_zoe, id_qls, id_sge, id_lia, id_rwa, id_tml, id_jmf,
            id_new).
         ((str_kcz$0 , str_klc) |-> str_cqf$0 >= TRUE)) <=>
      str_dxc$0 = str_vcb
    DO
      PRE
        not
          (!
             (id_ear, id_aiw, id_eqg, id_yjo, id_cbc, id_uiy, id_vlp, id_zqv,
              id_wlj, id_bet, id_kzi).
           (!(id_yzg).
            (!
               (id_zde, id_gia, id_any, id_zfu, id_myc, id_juc, id_zyx, id_ipx).
             (INT(str_npq$0) /=
              {INTEGER, str_cqy$0, INT, str_vdu, str_ywf$0, NATURAL1,
               str_uwq, str_uux}))))
      THEN
        WHILE
          not
            (rec
               (id_anv: [str_noz$0],
                id_ini: str_ryu$0,
                id_xec: str_ael,
                id_hym: str_spt$0,
                id_puz: bool(FALSE <: str_qyf$0 & str_rje$0 <<: TRUE),
                id_exi: INTEGER) /=
             str_knb$0'id_jcf)
        DO
          WHILE
            not
              (str_kse$0 /<<: str_gie &
               not(FALSE = str_mge$0 & MININT /= INTEGER) &
               !(id_wvd, id_axo, id_fcn, id_znn, id_htt, id_bhh, id_lid).
               (str_tio$0 > {}))
          DO
            id_vze 
              (
                UNION(id_wuu, id_mnv).
                (!(id_ujv, id_vmu).
                 (!(id_nib).
                  (!
                     (id_eyb, id_pex, id_znt, id_iyc, id_kha, id_ofa, id_szx,
                      id_uaa, id_skp, id_dvh, id_olf).
                   (INT > str_jzt$0)))|
                 struct
                   (id_ydt:
                    { id_rgs, id_qop, id_ydm, id_ujn, id_mgt, id_vxf, id_hxv,
                      id_pmd, id_oix
                      |
                      !
                        (id_sir, id_xyd, id_kis, id_dud, id_wgo, id_fql,
                         id_kut, id_heh, id_mtn, id_wrp, id_xsp).
                      (str_bpk <= {}) }))
              )
              (
                { id_fzp, id_uyh, id_qwz
                  |
                  #
                    (id_fzd, id_ikp, id_tgj, id_ttf, id_otk, id_zwh, id_lvh,
                     id_izi, id_paj, id_khc, id_aux).
                  (not
                     (#(id_swm, id_qck, id_thx, id_pqq).
                      (str_zdm$0 >= str_qab$0) or
                      FALSE = str_ptx$0) =>
                   !(id_iyt, id_gyz, id_uel, id_uyo).(str_tds$0 /<<: BOOL)) }
              )
              (
                (seq(MAXINT))
                ,
                rec
                  (id_wph: str_kau,
                   id_kqb: bool(str_gnu$0 <: str_dlo)'id_lkx,
                   id_ciz:
                   struct
                     (id_beb: str_rrd, id_rxx: str_lgo, id_vhp: str_mcl$0)
                     'id_vhj,
                   id_nzn: str_ufu$0,
                   id_jmh: {},
                   id_xji: str_meo$0,
                   id_aqz: str_jew)
              )
              (
                {str_ovh$0,
                 ((str_bgm |>> 9)(str_asv)),
                 str_edi'id_hkf'id_oon,
                 str_lmb,
                 str_pkb,
                 MAXINT,
                 [rec
                    (id_ipr: FALSE,
                     id_gxq: NAT,
                     id_oml: str_lzv$0,
                     id_dlc: str_ezn$0,
                     id_mjz: str_kys$0,
                     id_ttw: str_atp$0)]}
              )
            
            :=
            struct
              (id_yji: (str_var(FALSE)),
               id_cmk:
               INTER
                 (id_vio, id_dec, id_pdb, id_ccd, id_oso, id_xoc, id_qyz,
                  id_wiq).
               (TRUE /: BOOL| str_bxz$0),
               id_plv:
               struct
                 (id_stg: MAXINT,
                  id_cvr: str_xgu,
                  id_isv: str_gwv,
                  id_ekg: str_nse),
               id_ker: NAT1,
               id_agu: str_wke$0,
               id_wyy:
               struct
                 (id_ynv: {},
                  id_lnz: STRING,
                  id_emz: MAXINT,
                  id_lmr: MAXINT,
                  id_gjq: str_rir),
               id_nmp:
               { id_alp, id_nti, id_nar, id_uvj, id_wre, id_pqk, id_utg,
                 id_zas
                 |
                 str_rnf <<: BOOL or NATURAL1 /<: str_xzc })
          INVARIANT
            str_qnn$0
              (SIGMA
                 (id_bqf, id_fqz, id_fzo, id_fud, id_mzg, id_mmt, id_syi,
                  id_fgd).
               (!
                  (id_thz, id_kmd, id_nov, id_wbv, id_nah, id_egt, id_ysj,
                   id_llv, id_xjn, id_yek).
                (str_dhd > [])|
                str_qqb$0)) <
            bool(not(NATURAL /<<: []))
          VARIANT
            rec
              (id_azo: str_zap,
               id_wvf: "str_ysa",
               id_ppv: NATURAL,
               id_urq: str_vvs,
               id_jrk:
               %(id_dro, id_tje, id_deg). (str_ewi$0 >= str_tvv$0| STRING),
               id_jdq:
               { id_sho
                 |
                 not(NAT1 <<: str_gdf) &
                 (NATURAL : STRING <=>
                  #(id_abs, id_leg, id_cnu, id_ubp, id_nmm, id_hzp).
                  (NATURAL > str_krb$0)) },
               id_ujj: BOOL)
          END
        INVARIANT
          !(id_olt, id_whd, id_yym, id_quw, id_vuy, id_qel, id_arl).
          (#(id_hif, id_ekg, id_ycf, id_nym, id_jom).
           (#(id_umw, id_zqj, id_mqq, id_ddo, id_xys, id_pne).
            (!
               (id_gsa, id_vfs, id_cpu, id_moq, id_gve, id_loa, id_ufw,
                id_tbq, id_mlx, id_ujr).
             (not(not(str_pdq$0 : NATURAL1))))))
        VARIANT
          bool
            (#
               (id_xfb, id_kbp, id_enc, id_eay, id_scq, id_xhd, id_fea,
                id_lqh, id_dra, id_dsk, id_lfk).
             (not
                (struct
                   (id_lty: { id_ozs, id_geb | str_fri$0 < str_ucn },
                    id_vrd: str_zyv$0,
                    id_eyo: str_mwc$0,
                    id_xzm: str_rzr,
                    id_ytx: str_ubr$0,
                    id_ozo: str_doy,
                    id_sno:
                    {NAT, str_rqn, str_zmz$0, str_smz$0, str_feg, str_yea,
                     str_rak$0},
                    id_bpl: STRING) <
                 str_uke$0)))
        END
      END ;
      IF
        !(id_rjb, id_bdl, id_iru, id_dls, id_rgi).
        (#
           (id_xlz, id_ghd, id_lxe, id_jyf, id_utg, id_aqk, id_eog, id_sfv,
            id_zvp, id_jpf).
         (str_msv <= str_dir) <=>
         !(id_khe, id_qmo, id_eds, id_jga, id_boz, id_rnf, id_wyd).
         (not([] /: {str_mjq$0})))
      THEN
        id_rxv, id_bob, id_ncr
        <--
        id_pmo
          ([str_exz$0,
            bool
              (#(id_qay).
               (str_wfg$0 /<:
                str_hkp$0(rec(id_alt: "str_xyt", id_mqc: str_nla$0)))),
            str_asm$0,
            (str_vmi$0 , str_ajs)],
           [str_bxj$0,
            NAT,
            (((right(str_tgq)) |-> NAT)
             |->
             bool(str_qkw$0 <: str_yil$0 & str_riq > INT))],
           UNION
             (id_mvk, id_ifs, id_alk, id_hpa, id_xgw, id_mgm, id_urz, id_gzq,
              id_lfw, id_ppl).
           (NAT1 /: str_xfe|
            ((MININT
              ,
              INTER(id_oir, id_zpt, id_gxi, id_ieu, id_uyu, id_img).
              (str_jla /<: str_nna$0|
               {str_gyq$0,
                str_njc,
                rec
                  (id_tof: {},
                   id_pat: str_rky$0,
                   id_jyt: str_dgc$0,
                   id_zaw: str_lac,
                   id_kvx: str_ulp$0,
                   id_who: STRING,
                   id_ypy: MAXINT)}))
               (NATURAL))
              'id_ljt),
           struct
             (id_qfv:
              { id_ien, id_vbj
                |
                not
                  (!
                     (id_xyh, id_ncj, id_tzz, id_vyn, id_sdn, id_ulr, id_zxf,
                      id_gei).
                   (str_bey$0 < str_ucs$0)) },
              id_suo: str_rce,
              id_ior: NATURAL1,
              id_bjo: INT,
              id_ycb: str_nmu$0,
              id_lpp: (BOOL(NATURAL1)),
              id_sds: bool(str_voa /= str_cqi$0 <=> str_jwh /<<: str_yzm),
              id_acd: NAT1),
           ((bool
               (str_frg <=
                PI(id_xnw, id_kgy, id_qxc, id_otg, id_ngb, id_rsl, id_jny).
                (NAT /<: str_vfu| str_bbl$0))
               (STRING))
            |->
            { id_zht, id_okt, id_mtl, id_ipz, id_ibx, id_pio, id_zuw, id_exi
              |
              str_swg <= MAXINT }))
      ELSIF
        {(({str_dky$0, str_ctu, STRING, str_hjw$0, str_xdc, str_qcg$0}
             (UNION(id_ina). (NAT1 <<: str_mhu| BOOL))) /
          str_ntx$0'id_wcd),
         str_ovr$0,
         str_lhb$0,
         INT,
         str_egn} <:
        str_swd
      THEN
        id_xva, id_bns, id_owf, id_dvn, id_zsm, id_fwt, id_rqw, id_vtn,
        id_wcg, id_cbu
        ::
        rec
          (id_xzx: str_qaz$0,
           id_laj:
           PI
             (id_ipy, id_awz, id_tlw, id_ufi, id_sdk, id_mud, id_snd, id_agq,
              id_tij, id_ied, id_hcf).
           (str_xiu <<: INT|
            {str_ioy,
             [str_lsq,
              str_csp,
              (iterate(str_koh)),
              [],
              str_hur,
              (str_fqm /\ str_iaz)],
             str_lsb$0,
             NAT1,
             str_jhx,
             bool(MININT = []),
             str_bmz,
             str_vuc$0}),
           id_vzx: str_rnq$0)
      ELSIF
        !
          (id_umo, id_hje, id_dwh, id_bye, id_fwo, id_fqf, id_hsn, id_wer,
           id_nhn, id_kgb, id_ywe).
        (#(id_lfh).
         (not
            (not
               (str_njc /<: str_pfo or
                not(str_xto$0 /= str_fgk or MININT < str_gwa)))))
      THEN
        id_ruv, id_xqv, id_zyv, id_hoc, id_lns, id_dvl :(
          {str_hjs,
           str_oij,
           str_adh,
           FALSE,
           [],
           str_cbn$0,
           (str_ryb |-> str_pqn$0)} :
          %
            (id_yxb, id_jus, id_czl, id_ikf, id_bnz, id_pbz, id_btv, id_epm,
             id_wtp, id_xmn, id_qtc).
          (str_uyv$0 /: str_yvh$0 or str_ocb <<: MAXINT| str_ehl(TRUE))
        )
      ELSIF
        INTER(id_jos, id_sci, id_wsu). (str_uof$0 /<: {}| NATURAL) <<:
        { id_zag, id_qlg
          |
          !(id_jkp, id_xyu).(3 <= NATURAL) <=>
          not
            (!
               (id_wpo, id_vhw, id_fgr, id_rzh, id_ubu, id_kns, id_fav,
                id_nbz, id_sap, id_sga, id_dhc).
             (str_ddp >= str_ywo$0)) }
      THEN
        id_arc, id_rcq
        ::
        [str_rag,
         str_smm,
         struct
           (id_img: str_zak$0,
            id_zpp: (str_myx$0 --> (str_dzr$0 |-> str_ols$0)),
            id_xaa: str_hhd,
            id_zqw: str_qoc,
            id_qro: str_ice,
            id_okc: str_vsk$0,
            id_pjk:
            rec
              (id_wwi: NAT,
               id_xyz: NATURAL1,
               id_xla: str_aug$0,
               id_kax: str_mej$0,
               id_dyp: str_vad$0,
               id_jlc: (STRING , str_irw),
               id_ems: str_hqv$0,
               id_stt: [])),
         bool(str_qxg /<<: []),
         str_lcz]
      ELSE
        id_jgd, id_ldx :(
          #(id_hkq, id_xoo, id_isc, id_qcx, id_vdh, id_rqe, id_eoy, id_kim).
          ({ id_dgn, id_gsk, id_fbf, id_xfk, id_ibc, id_wgq
             |
             not
               (#(id_vii, id_ble, id_ufb, id_mgc, id_kil).
                (str_opq > str_ctk$0)) } >
           str_qjs(NATURAL))
        )
      END
    INVARIANT
      const
        (SIGMA
           (id_vai, id_vhf, id_ptc, id_oot, id_gpn, id_uvt, id_gjw, id_ffv).
         (TRUE <<: str_jzn$0| "str_xsy")) >
      bool
        (#(id_qlq, id_bzm, id_voc, id_ppw).
         (not(BOOL /<: str_hmt$0 or str_ggl /<<: str_tdy)))
    VARIANT
      (const
         ({({}
              (struct
                 (id_rsq: MAXINT,
                  id_prr: (FALSE(str_eyi)),
                  id_ige: str_lfd,
                  id_rgd: (bin(str_czh)))))}))
        'id_qxp
    END
  INVARIANT
    !(id_tra).
    (#(id_pit).
     (not
        (#
           (id_qys, id_qtt, id_wbw, id_ytt, id_yll, id_tak, id_lqb, id_omg,
            id_otw).
         (!(id_grx, id_ntk, id_ebs).
          (PI(id_opg). (str_oqe$0 /= MAXINT| str_jdp) /<<: str_qde)))))
  VARIANT
    bool
      (not
         ({ id_nad, id_fmo, id_pqr | str_krz$0 : str_oio } =
          (NAT(NATURAL)) ** str_cmk))
  END

END 