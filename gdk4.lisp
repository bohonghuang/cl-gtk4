;;;; gdk4.lisp

;;;; Copyright (C) 2022-2023 Bohong Huang
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(uiop:define-package gdk4
  (:use)
  (:use-reexport #:gdk-pixbuf2)
  (:shadow #:*ns*)
  (:nicknames #:gdk)
  (:export #:*ns*))

(cl:in-package #:gdk4)

(cl:eval-when (:execute :compile-toplevel :load-toplevel)
  (cl:setf gir-wrapper:*quoted-name-alist* '(("KEY_a"                         . |+KEY-a+|)
                                             ("KEY_ae"                        . |+KEY-ae+|)
                                             ("KEY_aacute"                    . |+KEY-aacute+|)
                                             ("KEY_abelowdot"                 . |+KEY-abelowdot+|)
                                             ("KEY_abreve"                    . |+KEY-abreve+|)
                                             ("KEY_abreveacute"               . |+KEY-abreveacute+|)
                                             ("KEY_abrevebelowdot"            . |+KEY-abrevebelowdot+|)
                                             ("KEY_abrevegrave"               . |+KEY-abrevegrave+|)
                                             ("KEY_abrevehook"                . |+KEY-abrevehook+|)
                                             ("KEY_abrevetilde"               . |+KEY-abrevetilde+|)
                                             ("KEY_acircumflex"               . |+KEY-acircumflex+|)
                                             ("KEY_acircumflexacute"          . |+KEY-acircumflexacute+|)
                                             ("KEY_acircumflexbelowdot"       . |+KEY-acircumflexbelowdot+|)
                                             ("KEY_acircumflexgrave"          . |+KEY-acircumflexgrave+|)
                                             ("KEY_acircumflexhook"           . |+KEY-acircumflexhook+|)
                                             ("KEY_acircumflextilde"          . |+KEY-acircumflextilde+|)
                                             ("KEY_adiaeresis"                . |+KEY-adiaeresis+|)
                                             ("KEY_agrave"                    . |+KEY-agrave+|)
                                             ("KEY_ahook"                     . |+KEY-ahook+|)
                                             ("KEY_amacron"                   . |+KEY-amacron+|)
                                             ("KEY_aogonek"                   . |+KEY-aogonek+|)
                                             ("KEY_aring"                     . |+KEY-aring+|)
                                             ("KEY_Armenian_at"               . |+KEY-ARMENIAN-at+|)
                                             ("KEY_Armenian_ayb"              . |+KEY-ARMENIAN-ayb+|)
                                             ("KEY_Armenian_ben"              . |+KEY-ARMENIAN-ben+|)
                                             ("KEY_Armenian_cha"              . |+KEY-ARMENIAN-cha+|)
                                             ("KEY_Armenian_da"               . |+KEY-ARMENIAN-da+|)
                                             ("KEY_Armenian_dza"              . |+KEY-ARMENIAN-dza+|)
                                             ("KEY_Armenian_e"                . |+KEY-ARMENIAN-e+|)
                                             ("KEY_Armenian_fe"               . |+KEY-ARMENIAN-fe+|)
                                             ("KEY_Armenian_ghat"             . |+KEY-ARMENIAN-ghat+|)
                                             ("KEY_Armenian_gim"              . |+KEY-ARMENIAN-gim+|)
                                             ("KEY_Armenian_hi"               . |+KEY-ARMENIAN-hi+|)
                                             ("KEY_Armenian_ho"               . |+KEY-ARMENIAN-ho+|)
                                             ("KEY_Armenian_ini"              . |+KEY-ARMENIAN-ini+|)
                                             ("KEY_Armenian_je"               . |+KEY-ARMENIAN-je+|)
                                             ("KEY_Armenian_ke"               . |+KEY-ARMENIAN-ke+|)
                                             ("KEY_Armenian_ken"              . |+KEY-ARMENIAN-ken+|)
                                             ("KEY_Armenian_khe"              . |+KEY-ARMENIAN-khe+|)
                                             ("KEY_Armenian_lyun"             . |+KEY-ARMENIAN-lyun+|)
                                             ("KEY_Armenian_men"              . |+KEY-ARMENIAN-men+|)
                                             ("KEY_Armenian_nu"               . |+KEY-ARMENIAN-nu+|)
                                             ("KEY_Armenian_o"                . |+KEY-ARMENIAN-o+|)
                                             ("KEY_Armenian_pe"               . |+KEY-ARMENIAN-pe+|)
                                             ("KEY_Armenian_pyur"             . |+KEY-ARMENIAN-pyur+|)
                                             ("KEY_Armenian_ra"               . |+KEY-ARMENIAN-ra+|)
                                             ("KEY_Armenian_re"               . |+KEY-ARMENIAN-re+|)
                                             ("KEY_Armenian_se"               . |+KEY-ARMENIAN-se+|)
                                             ("KEY_Armenian_sha"              . |+KEY-ARMENIAN-sha+|)
                                             ("KEY_Armenian_tche"             . |+KEY-ARMENIAN-tche+|)
                                             ("KEY_Armenian_to"               . |+KEY-ARMENIAN-to+|)
                                             ("KEY_Armenian_tsa"              . |+KEY-ARMENIAN-tsa+|)
                                             ("KEY_Armenian_tso"              . |+KEY-ARMENIAN-tso+|)
                                             ("KEY_Armenian_tyun"             . |+KEY-ARMENIAN-tyun+|)
                                             ("KEY_Armenian_vev"              . |+KEY-ARMENIAN-vev+|)
                                             ("KEY_Armenian_vo"               . |+KEY-ARMENIAN-vo+|)
                                             ("KEY_Armenian_vyun"             . |+KEY-ARMENIAN-vyun+|)
                                             ("KEY_Armenian_yech"             . |+KEY-ARMENIAN-yech+|)
                                             ("KEY_Armenian_za"               . |+KEY-ARMENIAN-za+|)
                                             ("KEY_Armenian_zhe"              . |+KEY-ARMENIAN-zhe+|)
                                             ("KEY_atilde"                    . |+KEY-atilde+|)
                                             ("KEY_b"                         . |+KEY-b+|)
                                             ("KEY_babovedot"                 . |+KEY-babovedot+|)
                                             ("KEY_Byelorussian_shortu"       . |+KEY-BYELORUSSIAN-shortu+|)
                                             ("KEY_c"                         . |+KEY-c+|)
                                             ("KEY_Ch"                        . |+KEY-Ch+|)
                                             ("KEY_ch"                        . |+KEY-ch+|)
                                             ("KEY_C_h"                       . |+KEY-C-h+|)
                                             ("KEY_c_h"                       . |+KEY-c-h+|)
                                             ("KEY_cabovedot"                 . |+KEY-cabovedot+|)
                                             ("KEY_cacute"                    . |+KEY-cacute+|)
                                             ("KEY_ccaron"                    . |+KEY-ccaron+|)
                                             ("KEY_ccedilla"                  . |+KEY-ccedilla+|)
                                             ("KEY_ccircumflex"               . |+KEY-ccircumflex+|)
                                             ("KEY_ch"                        . |+KEY-ch+|)
                                             ("KEY_Cyrillic_a"                . |+KEY-CYRILLIC-a+|)
                                             ("KEY_Cyrillic_be"               . |+KEY-CYRILLIC-be+|)
                                             ("KEY_Cyrillic_che"              . |+KEY-CYRILLIC-che+|)
                                             ("KEY_Cyrillic_che_descender"    . |+KEY-CYRILLIC_che-DESCENDER+|)
                                             ("KEY_Cyrillic_che_vertstroke"   . |+KEY-CYRILLIC_che-VERTSTROKE+|)
                                             ("KEY_Cyrillic_de"               . |+KEY-CYRILLIC-de+|)
                                             ("KEY_Cyrillic_dzhe"             . |+KEY-CYRILLIC-dzhe+|)
                                             ("KEY_Cyrillic_e"                . |+KEY-CYRILLIC-e+|)
                                             ("KEY_Cyrillic_ef"               . |+KEY-CYRILLIC-ef+|)
                                             ("KEY_Cyrillic_el"               . |+KEY-CYRILLIC-el+|)
                                             ("KEY_Cyrillic_em"               . |+KEY-CYRILLIC-em+|)
                                             ("KEY_Cyrillic_en"               . |+KEY-CYRILLIC-en+|)
                                             ("KEY_Cyrillic_en_descender"     . |+KEY-CYRILLIC_en-DESCENDER+|)
                                             ("KEY_Cyrillic_er"               . |+KEY-CYRILLIC-er+|)
                                             ("KEY_Cyrillic_es"               . |+KEY-CYRILLIC-es+|)
                                             ("KEY_Cyrillic_ghe"              . |+KEY-CYRILLIC-ghe+|)
                                             ("KEY_Cyrillic_ghe_bar"          . |+KEY-CYRILLIC_ghe-BAR+|)
                                             ("KEY_Cyrillic_ha"               . |+KEY-CYRILLIC-ha+|)
                                             ("KEY_Cyrillic_hardsign"         . |+KEY-CYRILLIC-hardsign+|)
                                             ("KEY_Cyrillic_ha_descender"     . |+KEY-CYRILLIC_ha-DESCENDER+|)
                                             ("KEY_Cyrillic_i"                . |+KEY-CYRILLIC-i+|)
                                             ("KEY_Cyrillic_ie"               . |+KEY-CYRILLIC-ie+|)
                                             ("KEY_Cyrillic_io"               . |+KEY-CYRILLIC-io+|)
                                             ("KEY_Cyrillic_i_macron"         . |+KEY-CYRILLIC_i-MACRON+|)
                                             ("KEY_Cyrillic_je"               . |+KEY-CYRILLIC-je+|)
                                             ("KEY_Cyrillic_ka"               . |+KEY-CYRILLIC-ka+|)
                                             ("KEY_Cyrillic_ka_descender"     . |+KEY-CYRILLIC-ka-DESCENDER+|)
                                             ("KEY_Cyrillic_ka_vertstroke"    . |+KEY-CYRILLIC-ka-VERTSTROKE+|)
                                             ("KEY_Cyrillic_lje"              . |+KEY-CYRILLIC-lje+|)
                                             ("KEY_Cyrillic_nje"              . |+KEY-CYRILLIC-nje+|)
                                             ("KEY_Cyrillic_o"                . |+KEY-CYRILLIC-o+|)
                                             ("KEY_Cyrillic_o_bar"            . |+KEY-CYRILLIC_O-bar+|)
                                             ("KEY_Cyrillic_pe"               . |+KEY-CYRILLIC-pe+|)
                                             ("KEY_Cyrillic_schwa"            . |+KEY-CYRILLIC-schwa+|)
                                             ("KEY_Cyrillic_sha"              . |+KEY-CYRILLIC-sha+|)
                                             ("KEY_Cyrillic_shcha"            . |+KEY-CYRILLIC-shcha+|)
                                             ("KEY_Cyrillic_shha"             . |+KEY-CYRILLIC-shha+|)
                                             ("KEY_Cyrillic_shorti"           . |+KEY-CYRILLIC-shorti+|)
                                             ("KEY_Cyrillic_softsign"         . |+KEY-CYRILLIC-softsign+|)
                                             ("KEY_Cyrillic_te"               . |+KEY-CYRILLIC-te+|)
                                             ("KEY_Cyrillic_tse"              . |+KEY-CYRILLIC-tse+|)
                                             ("KEY_Cyrillic_u"                . |+KEY-CYRILLIC-u+|)
                                             ("KEY_Cyrillic_u_macron"         . |+KEY-CYRILLIC-u-MACRON+|)
                                             ("KEY_Cyrillic_u_straight"       . |+KEY-CYRILLIC-u-STRAIGHT+|)
                                             ("KEY_Cyrillic_u_straight_bar"   . |+KEY-CYRILLIC-u-STRAIGHT-BAR+|)
                                             ("KEY_Cyrillic_ve"               . |+KEY-CYRILLIC-ve+|)
                                             ("KEY_Cyrillic_ya"               . |+KEY-CYRILLIC-ya+|)
                                             ("KEY_Cyrillic_yeru"             . |+KEY-CYRILLIC-yeru+|)
                                             ("KEY_Cyrillic_yu"               . |+KEY-CYRILLIC-yu+|)
                                             ("KEY_Cyrillic_ze"               . |+KEY-CYRILLIC-ze+|)
                                             ("KEY_Cyrillic_zhe"              . |+KEY-CYRILLIC-zhe+|)
                                             ("KEY_Cyrillic_zhe_descender"    . |+KEY-CYRILLIC_ZHE-descender+|)
                                             ("KEY_d"                         . |+KEY-d+|)
                                             ("KEY_dabovedot"                 . |+KEY-dabovedot+|)
                                             ("KEY_dcaron"                    . |+KEY-dcaron+|)
                                             ("KEY_dstroke"                   . |+KEY-dstroke+|)
                                             ("KEY_e"                         . |+KEY-e+|)
                                             ("KEY_eng"                       . |+KEY-eng+|)
                                             ("KEY_eth"                       . |+KEY-eth+|)
                                             ("KEY_ezh"                       . |+KEY-ezh+|)
                                             ("KEY_eabovedot"                 . |+KEY-eabovedot+|)
                                             ("KEY_eacute"                    . |+KEY-eacute+|)
                                             ("KEY_ebelowdot"                 . |+KEY-ebelowdot+|)
                                             ("KEY_ecaron"                    . |+KEY-ecaron+|)
                                             ("KEY_ecircumflex"               . |+KEY-ecircumflex+|)
                                             ("KEY_ecircumflexacute"          . |+KEY-ecircumflexacute+|)
                                             ("KEY_ecircumflexbelowdot"       . |+KEY-ecircumflexbelowdot+|)
                                             ("KEY_ecircumflexgrave"          . |+KEY-ecircumflexgrave+|)
                                             ("KEY_ecircumflexhook"           . |+KEY-ecircumflexhook+|)
                                             ("KEY_ecircumflextilde"          . |+KEY-ecircumflextilde+|)
                                             ("KEY_ediaeresis"                . |+KEY-ediaeresis+|)
                                             ("KEY_egrave"                    . |+KEY-egrave+|)
                                             ("KEY_ehook"                     . |+KEY-ehook+|)
                                             ("KEY_emacron"                   . |+KEY-emacron+|)
                                             ("KEY_eogonek"                   . |+KEY-eogonek+|)
                                             ("KEY_eth"                       . |+KEY-eth+|)
                                             ("KEY_etilde"                    . |+KEY-etilde+|)
                                             ("KEY_f"                         . |+KEY-f+|)
                                             ("KEY_fabovedot"                 . |+KEY-fabovedot+|)
                                             ("KEY_g"                         . |+KEY-g+|)
                                             ("KEY_gabovedot"                 . |+KEY-gabovedot+|)
                                             ("KEY_gbreve"                    . |+KEY-gbreve+|)
                                             ("KEY_gcaron"                    . |+KEY-gcaron+|)
                                             ("KEY_gcedilla"                  . |+KEY-gcedilla+|)
                                             ("KEY_gcircumflex"               . |+KEY-gcircumflex+|)
                                             ("KEY_Greek_alpha"               . |+KEY-GREEK-alpha+|)
                                             ("KEY_Greek_alphaaccent"         . |+KEY-GREEK-alphaaccent+|)
                                             ("KEY_Greek_beta"                . |+KEY-GREEK-beta+|)
                                             ("KEY_Greek_chi"                 . |+KEY-GREEK-chi+|)
                                             ("KEY_Greek_delta"               . |+KEY-GREEK-delta+|)
                                             ("KEY_Greek_epsilon"             . |+KEY-GREEK-epsilon+|)
                                             ("KEY_Greek_epsilonaccent"       . |+KEY-GREEK-epsilonaccent+|)
                                             ("KEY_Greek_eta"                 . |+KEY-GREEK-eta+|)
                                             ("KEY_Greek_etaaccent"           . |+KEY-GREEK-etaaccent+|)
                                             ("KEY_Greek_gamma"               . |+KEY-GREEK-gamma+|)
                                             ("KEY_Greek_iota"                . |+KEY-GREEK-iota+|)
                                             ("KEY_Greek_iotaaccent"          . |+KEY-GREEK-iotaaccent+|)
                                             ("KEY_Greek_iotadieresis"        . |+KEY-GREEK-iotadieresis+|)
                                             ("KEY_Greek_kappa"               . |+KEY-GREEK-kappa+|)
                                             ("KEY_Greek_lambda"              . |+KEY-GREEK-lambda+|)
                                             ("KEY_Greek_lamda"               . |+KEY-GREEK-lamda+|)
                                             ("KEY_Greek_mu"                  . |+KEY-GREEK-mu+|)
                                             ("KEY_Greek_nu"                  . |+KEY-GREEK-nu+|)
                                             ("KEY_Greek_omega"               . |+KEY-GREEK-omega+|)
                                             ("KEY_Greek_omegaaccent"         . |+KEY-GREEK-omegaaccent+|)
                                             ("KEY_Greek_omicron"             . |+KEY-GREEK-omicron+|)
                                             ("KEY_Greek_omicronaccent"       . |+KEY-GREEK-omicronaccent+|)
                                             ("KEY_Greek_phi"                 . |+KEY-GREEK-phi+|)
                                             ("KEY_Greek_pi"                  . |+KEY-GREEK-pi+|)
                                             ("KEY_Greek_psi"                 . |+KEY-GREEK-psi+|)
                                             ("KEY_Greek_rho"                 . |+KEY-GREEK-rho+|)
                                             ("KEY_Greek_sigma"               . |+KEY-GREEK-sigma+|)
                                             ("KEY_Greek_tau"                 . |+KEY-GREEK-tau+|)
                                             ("KEY_Greek_theta"               . |+KEY-GREEK-theta+|)
                                             ("KEY_Greek_upsilon"             . |+KEY-GREEK-upsilon+|)
                                             ("KEY_Greek_upsilonaccent"       . |+KEY-GREEK-upsilonaccent+|)
                                             ("KEY_Greek_upsilondieresis"     . |+KEY-GREEK-upsilondieresis+|)
                                             ("KEY_Greek_xi"                  . |+KEY-GREEK-xi+|)
                                             ("KEY_Greek_zeta"                . |+KEY-GREEK-zeta+|)
                                             ("KEY_h"                         . |+KEY-h+|)
                                             ("KEY_hcircumflex"               . |+KEY-hcircumflex+|)
                                             ("KEY_hstroke"                   . |+KEY-hstroke+|)
                                             ("KEY_i"                         . |+KEY-i+|)
                                             ("KEY_iacute"                    . |+KEY-iacute+|)
                                             ("KEY_ibelowdot"                 . |+KEY-ibelowdot+|)
                                             ("KEY_ibreve"                    . |+KEY-ibreve+|)
                                             ("KEY_icircumflex"               . |+KEY-icircumflex+|)
                                             ("KEY_idiaeresis"                . |+KEY-idiaeresis+|)
                                             ("KEY_igrave"                    . |+KEY-igrave+|)
                                             ("KEY_ihook"                     . |+KEY-ihook+|)
                                             ("KEY_imacron"                   . |+KEY-imacron+|)
                                             ("KEY_iogonek"                   . |+KEY-iogonek+|)
                                             ("KEY_itilde"                    . |+KEY-itilde+|)
                                             ("KEY_j"                         . |+KEY-j+|)
                                             ("KEY_jcircumflex"               . |+KEY-jcircumflex+|)
                                             ("KEY_k"                         . |+KEY-k+|)
                                             ("KEY_kcedilla"                  . |+KEY-kcedilla+|)
                                             ("KEY_l"                         . |+KEY-l+|)
                                             ("KEY_lacute"                    . |+KEY-lacute+|)
                                             ("KEY_lbelowdot"                 . |+KEY-lbelowdot+|)
                                             ("KEY_lcaron"                    . |+KEY-lcaron+|)
                                             ("KEY_lcedilla"                  . |+KEY-lcedilla+|)
                                             ("KEY_lstroke"                   . |+KEY-lstroke+|)
                                             ("KEY_m"                         . |+KEY-m+|)
                                             ("KEY_mabovedot"                 . |+KEY-mabovedot+|)
                                             ("KEY_Macedonia_dse"             . |+KEY-MACEDONIA-dse+|)
                                             ("KEY_Macedonia_gje"             . |+KEY-MACEDONIA-gje+|)
                                             ("KEY_Macedonia_kje"             . |+KEY-MACEDONIA-kje+|)
                                             ("KEY_n"                         . |+KEY-n+|)
                                             ("KEY_nacute"                    . |+KEY-nacute+|)
                                             ("KEY_ncaron"                    . |+KEY-ncaron+|)
                                             ("KEY_ncedilla"                  . |+KEY-ncedilla+|)
                                             ("KEY_ntilde"                    . |+KEY-ntilde+|)
                                             ("KEY_o"                         . |+KEY-o+|)
                                             ("KEY_oe"                        . |+KEY-oe+|)
                                             ("KEY_oacute"                    . |+KEY-oacute+|)
                                             ("KEY_obarred"                   . |+KEY-obarred+|)
                                             ("KEY_obelowdot"                 . |+KEY-obelowdot+|)
                                             ("KEY_ocaron"                    . |+KEY-ocaron+|)
                                             ("KEY_ocircumflex"               . |+KEY-ocircumflex+|)
                                             ("KEY_ocircumflexacute"          . |+KEY-ocircumflexacute+|)
                                             ("KEY_ocircumflexbelowdot"       . |+KEY-ocircumflexbelowdot+|)
                                             ("KEY_ocircumflexgrave"          . |+KEY-ocircumflexgrave+|)
                                             ("KEY_ocircumflexhook"           . |+KEY-ocircumflexhook+|)
                                             ("KEY_ocircumflextilde"          . |+KEY-ocircumflextilde+|)
                                             ("KEY_odiaeresis"                . |+KEY-odiaeresis+|)
                                             ("KEY_odoubleacute"              . |+KEY-odoubleacute+|)
                                             ("KEY_ograve"                    . |+KEY-ograve+|)
                                             ("KEY_ohook"                     . |+KEY-ohook+|)
                                             ("KEY_ohorn"                     . |+KEY-ohorn+|)
                                             ("KEY_ohornacute"                . |+KEY-ohornacute+|)
                                             ("KEY_ohornbelowdot"             . |+KEY-ohornbelowdot+|)
                                             ("KEY_ohorngrave"                . |+KEY-ohorngrave+|)
                                             ("KEY_ohornhook"                 . |+KEY-ohornhook+|)
                                             ("KEY_ohorntilde"                . |+KEY-ohorntilde+|)
                                             ("KEY_omacron"                   . |+KEY-omacron+|)
                                             ("KEY_ooblique"                  . |+KEY-ooblique+|)
                                             ("KEY_oslash"                    . |+KEY-oslash+|)
                                             ("KEY_otilde"                    . |+KEY-otilde+|)
                                             ("KEY_p"                         . |+KEY-p+|)
                                             ("KEY_pabovedot"                 . |+KEY-pabovedot+|)
                                             ("KEY_q"                         . |+KEY-q+|)
                                             ("KEY_r"                         . |+KEY-r+|)
                                             ("KEY_racute"                    . |+KEY-racute+|)
                                             ("KEY_rcaron"                    . |+KEY-rcaron+|)
                                             ("KEY_rcedilla"                  . |+KEY-rcedilla+|)
                                             ("KEY_s"                         . |+KEY-s+|)
                                             ("KEY_schwa"                     . |+KEY-schwa+|)
                                             ("KEY_sabovedot"                 . |+KEY-sabovedot+|)
                                             ("KEY_sacute"                    . |+KEY-sacute+|)
                                             ("KEY_scaron"                    . |+KEY-scaron+|)
                                             ("KEY_scedilla"                  . |+KEY-scedilla+|)
                                             ("KEY_scircumflex"               . |+KEY-scircumflex+|)
                                             ("KEY_Serbian_dje"               . |+KEY-SERBIAN-dje+|)
                                             ("KEY_Serbian_dze"               . |+KEY-SERBIAN-dze+|)
                                             ("KEY_Serbian_je"                . |+KEY-SERBIAN-je+|)
                                             ("KEY_Serbian_lje"               . |+KEY-SERBIAN-lje+|)
                                             ("KEY_Serbian_nje"               . |+KEY-SERBIAN-nje+|)
                                             ("KEY_Serbian_tshe"              . |+KEY-SERBIAN-tshe+|)
                                             ("KEY_ScreenSaver"               . |+KEY-screensaver+|)
                                             ("KEY_t"                         . |+KEY-t+|)
                                             ("KEY_thorn"                     . |+KEY-thorn+|)
                                             ("KEY_tabovedot"                 . |+KEY-tabovedot+|)
                                             ("KEY_tcaron"                    . |+KEY-tcaron+|)
                                             ("KEY_tcedilla"                  . |+KEY-tcedilla+|)
                                             ("KEY_thorn"                     . |+KEY-thorn+|)
                                             ("KEY_tslash"                    . |+KEY-tslash+|)
                                             ("KEY_u"                         . |+KEY-u+|)
                                             ("KEY_uacute"                    . |+KEY-uacute+|)
                                             ("KEY_ubelowdot"                 . |+KEY-ubelowdot+|)
                                             ("KEY_ubreve"                    . |+KEY-ubreve+|)
                                             ("KEY_ucircumflex"               . |+KEY-ucircumflex+|)
                                             ("KEY_udiaeresis"                . |+KEY-udiaeresis+|)
                                             ("KEY_udoubleacute"              . |+KEY-udoubleacute+|)
                                             ("KEY_ugrave"                    . |+KEY-ugrave+|)
                                             ("KEY_uhook"                     . |+KEY-uhook+|)
                                             ("KEY_uhorn"                     . |+KEY-uhorn+|)
                                             ("KEY_uhornacute"                . |+KEY-uhornacute+|)
                                             ("KEY_uhornbelowdot"             . |+KEY-uhornbelowdot+|)
                                             ("KEY_uhorngrave"                . |+KEY-uhorngrave+|)
                                             ("KEY_uhornhook"                 . |+KEY-uhornhook+|)
                                             ("KEY_uhorntilde"                . |+KEY-uhorntilde+|)
                                             ("KEY_Ukrainian_ghe_with_upturn" . |+KEY-UKRAINIAN-ghe-WITH-UPTURN+|)
                                             ("KEY_Ukrainian_i"               . |+KEY-UKRAINIAN-i+|)
                                             ("KEY_Ukrainian_ie"              . |+KEY-UKRAINIAN-ie+|)
                                             ("KEY_Ukrainian_yi"              . |+KEY-UKRAINIAN-yi+|)
                                             ("KEY_Ukranian_i"                . |+KEY-UKRANIAN-i+|)
                                             ("KEY_Ukranian_je"               . |+KEY-UKRANIAN-je+|)
                                             ("KEY_Ukranian_yi"               . |+KEY-UKRANIAN-yi+|)
                                             ("KEY_umacron"                   . |+KEY-umacron+|)
                                             ("KEY_uogonek"                   . |+KEY-uogonek+|)
                                             ("KEY_uring"                     . |+KEY-uring+|)
                                             ("KEY_utilde"                    . |+KEY-utilde+|)
                                             ("KEY_v"                         . |+KEY-v+|)
                                             ("KEY_w"                         . |+KEY-w+|)
                                             ("KEY_wacute"                    . |+KEY-wacute+|)
                                             ("KEY_wcircumflex"               . |+KEY-wcircumflex+|)
                                             ("KEY_wdiaeresis"                . |+KEY-wdiaeresis+|)
                                             ("KEY_wgrave"                    . |+KEY-wgrave+|)
                                             ("KEY_x"                         . |+KEY-x+|)
                                             ("KEY_xabovedot"                 . |+KEY-xabovedot+|)
                                             ("KEY_y"                         . |+KEY-y+|)
                                             ("KEY_yacute"                    . |+KEY-yacute+|)
                                             ("KEY_ybelowdot"                 . |+KEY-ybelowdot+|)
                                             ("KEY_ycircumflex"               . |+KEY-ycircumflex+|)
                                             ("KEY_ydiaeresis"                . |+KEY-ydiaeresis+|)
                                             ("KEY_ygrave"                    . |+KEY-ygrave+|)
                                             ("KEY_yhook"                     . |+KEY-yhook+|)
                                             ("KEY_ytilde"                    . |+KEY-ytilde+|)
                                             ("KEY_z"                         . |+KEY-z+|)
                                             ("KEY_zabovedot"                 . |+KEY-zabovedot+|)
                                             ("KEY_zacute"                    . |+KEY-zacute+|)
                                             ("KEY_zcaron"                    . |+KEY-zcaron+|)
                                             ("KEY_zstroke"                   . |+KEY-zstroke+|)
                                             ("KEY_dead_a"                    . |+KEY-DEAD-a+|)
                                             ("KEY_dead_e"                    . |+KEY-DEAD-e+|)
                                             ("KEY_dead_i"                    . |+KEY-DEAD-i+|)
                                             ("KEY_dead_o"                    . |+KEY-DEAD-o+|)
                                             ("KEY_dead_u"                    . |+KEY-DEAD-u+|)
                                             ("KEY_dead_schwa"                . |+KEY-DEAD-schwa+|)
                                             ("KEY_kana_a"                    . |+KEY-KANA-a+|)
                                             ("KEY_kana_e"                    . |+KEY-KANA-e+|)
                                             ("KEY_kana_i"                    . |+KEY-KANA-i+|)
                                             ("KEY_kana_o"                    . |+KEY-KANA-o+|)
                                             ("KEY_kana_tsu"                  . |+KEY-KANA-tsu+|)
                                             ("KEY_kana_tu"                   . |+KEY-KANA-tu+|)
                                             ("KEY_kana_u"                    . |+KEY-KANA-u+|)
                                             ("KEY_kana_ya"                   . |+KEY-KANA-ya+|)
                                             ("KEY_kana_yo"                   . |+KEY-KANA-yo+|)
                                             ("KEY_kana_yu"                   . |+KEY-KANA-yu+|))))

(gir-wrapper:define-gir-namespace "Gdk" "4.0")

(cffi:defcstruct rectangle
  "A GdkRectangle data type for representing rectangles."
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(cl:eval-when (:execute :compile-toplevel :load-toplevel)
  (cl:setf gir-wrapper:*quoted-name-alist* cl:nil))
