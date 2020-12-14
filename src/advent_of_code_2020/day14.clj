(ns advent-of-code-2020.day14
  (:require [clojure.string :as s]
            [clojure.walk :refer [postwalk-replace]]))

(defmacro cond-matches
  "Takes a string s and a series of regular expression/expression pairs. s is
   matched against the regular expressions in order and the expression associated
   with the first matching regular expression is evaluated with the
   keyword :match being replaced by the match data as in re-groups.

   A trailing default expression without a preceding regular expression is
   evaluated if none of the regular expressions match. :match is not expanded in
   the default expression (because there is not match).

   If no default expression is given and none of the regular expression matches,
   cond-matches returns nil."
  {:style/indent 1}
  ([s re expr & rest]
   (let [match (gensym "match")]
     `(if-let [~match (re-matches ~re ~s)]
        ~(postwalk-replace {:match match} expr)
        ~(when rest
           `(cond-matches ~s ~@rest)))))
  ([s expr]
   expr))

(defn- parse-instruction
  "Parses a single instruction from the initialization program."
  [s]
  (cond-matches s
    #"mask\s*=\s*([01X]{36})"
    {:operation :set-mask
     :arguments [(second :match)]}
    #"mem\[(\d+)]\s*=\s*(\d+)"
    {:operation :set-mem
     :arguments [(BigInteger. (nth :match 1))
                 (BigInteger. (nth :match 2))]}
    (throw (IllegalArgumentException. (str "Invalid input line: " s)))))

(defn- parse-initialization-program
  "Parses the initialization program."
  [s]
  (->> s
       s/split-lines
       (map parse-instruction)))

(defn- interpret-mask-v1
  "Returns an executable interpretation of mask as per version 1 of the decoder chip."
  [mask]
  (let [zero-mask (BigInteger. (s/replace mask "X" "1") 2)
        one-mask (BigInteger. (s/replace mask "X" "0") 2)]
    #(-> %
         (.and zero-mask)
         (.or one-mask))))

(def operations-v1
  "Maps operations to functions executing them on state and [arguments] as per
   version 1 of the decoder chip."
  {:set-mask #(assoc %1 :mask (interpret-mask-v1 (first %2)))
   :set-mem #(assoc-in %1 [:memory (first %2)] ((get %1 :mask) (second %2)))})

(defn- run-initialization-program
  "Runs the given initialization program on the given memory state and returns the
   resulting memory state."
  [operations program]
  (reduce (fn [state instruction]
            ((operations (instruction :operation)) state (instruction :arguments)))
          {}
          program))

(defn- sum-memory
  "Returns the sum of all values in the given memory state."
  [state]
  (apply + (vals (state :memory))))

(defn solve-1
  "Returns the sum of all values left in memory after the initialization program
   in s completes according to version 1 of the decoder chip."
  [s]
  (->> s
       parse-initialization-program
       (run-initialization-program operations-v1)
       sum-memory))

(defn- expand-floating-bits
  "Takes a value and returns a sequence of values in which the floating bits are
   applied in all possible combinations."
  [value floating-bits]
  (loop [values (list value)
         floating-bits floating-bits]
    (if (empty? floating-bits)
      values
      (recur (let [^Long bit (get floating-bits 0)]
               (reduce conj values (map (fn [^BigInteger v] (.clearBit v bit)) values)))
             (subvec floating-bits 1)))))

(defn- interpret-mask-v2
  "Returns an executable interpretation of mask as per version 2 of the decoder chip."
  [mask]
  (let [one-mask (BigInteger. (s/replace mask \X \1) 2)
        floating-bits (->> mask reverse (map-indexed vector) (filter #(= (second %) \X)) (map first) vec)]
    #(-> %
         (.or one-mask)
         (expand-floating-bits floating-bits))))

(def operations-v2
  "Maps operations to functions executing them on state and [arguments] as per
   version 2 of the decoder chip."
  {:set-mask #(assoc %1 :mask (interpret-mask-v2 (first %2)))
   :set-mem (fn [state arguments]
              (reduce #(assoc-in %1 [:memory %2] (second arguments))
                      state
                      ((get state :mask) (first arguments))))})

(defn solve-2
  "Returns the sum of all values left in memory after the initialization program
   in s completes according to version 2 of the decoder chip."
  [s]
  (->> s
       parse-initialization-program
       (run-initialization-program operations-v2)
       sum-memory))

(def trial-input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(def trial-input-2 "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")

(def real-input "mask = 110100X1X01011X01X0X000111X00XX1010X
mem[29267] = 4155
mem[6177] = 494929
mem[47500] = 15063410
mask = 11110X000010XX10X11X00X11010X0X00101
mem[10164] = 73599
mem[61707] = 15191
mem[53825] = 69618638
mem[15953] = 10067309
mem[39889] = 10761258
mask = 11XXX0XX001X01101111X001000X01X10000
mem[6917] = 2088
mem[10383] = 52991144
mem[14304] = 84876
mem[6464] = 14167
mask = X11X0X00001010101XX101100X011X000XX1
mem[58256] = 51753803
mem[2919] = 21619
mask = 111100X00010101011X1XXXXX01X000001X1
mem[13063] = 6587
mem[28673] = 3893
mem[53317] = 236744
mem[64240] = 29367045
mask = XX11101X0011X01X1111X001X1X000101110
mem[59497] = 6945823
mem[44552] = 193130494
mem[15034] = 10641
mem[25467] = 17061678
mask = X11110X11X11X000110110010110011X0001
mem[22485] = 90244
mem[38996] = 221133
mem[15034] = 12927587
mem[4425] = 31960477
mem[18544] = 51522085
mask = 111100100010X0101XX1110100000XXX0001
mem[53765] = 1528530
mem[18576] = 68702
mem[6484] = 170802
mem[62813] = 857
mask = 1X1101010X1X111X110101X1010100010010
mem[34688] = 36117345
mem[12832] = 14851833
mem[12097] = 43
mem[63931] = 875829
mask = 10XXX01100101010X1X111010000101101X1
mem[33582] = 28611764
mem[53969] = 2641
mem[53107] = 99965
mem[40648] = 1834153
mem[55768] = 974
mem[52274] = 7634
mask = 111010X10010111X1111X0XXX00110110001
mem[43784] = 12232
mem[23109] = 11403
mem[18521] = 145694
mem[12479] = 621
mask = XX000011001000101111XX101X1X10101X11
mem[22233] = 30348
mem[7554] = 346199
mem[1904] = 12735
mem[23360] = 73797
mem[22803] = 62630115
mask = 1110X0X0X010X01011X10110001X00XX0110
mem[17347] = 310008019
mem[39889] = 8179468
mem[57304] = 861379
mem[2513] = 10343
mask = 1111X00100101X1X110101111111010000XX
mem[49939] = 39353857
mem[48794] = 4266
mem[16061] = 2768
mem[7728] = 103608
mem[17473] = 8980
mask = 1001XXX10X0X11X0010100001100X0110101
mem[21497] = 412878
mem[49939] = 429971
mem[3405] = 20518
mem[6160] = 51622
mem[43784] = 179922643
mask = 101100X1001X10XX1101X00XX0101000100X
mem[52378] = 1530
mem[11326] = 121760772
mem[58232] = 164870285
mem[21604] = 17658898
mem[56257] = 373
mem[37783] = 7760640
mem[3927] = 427054839
mask = 1111XXX10010X11X110101011X10XX10000X
mem[18153] = 234818
mem[56755] = 531
mem[50940] = 4428
mem[14849] = 2185318
mask = 10110001X0101010X101010X10000X110001
mem[53880] = 33275
mem[61621] = 40021
mem[10319] = 9468
mem[34607] = 1186
mem[43043] = 51628146
mem[14912] = 2626679
mem[4627] = 377
mask = 111X0X00XXX01110011X10001X1110010101
mem[39892] = 6297
mem[13182] = 4721
mem[41736] = 497649
mem[40923] = 905
mask = 1X1X00110010011X1110X0001X10011XXX00
mem[27118] = 898
mem[59800] = 499005720
mask = 11X00101001011100X01X01XX100XX010110
mem[56505] = 23945612
mem[13987] = 14200131
mem[30557] = 10396
mask = 1110000100X1X01X011111X001101011010X
mem[22942] = 642242
mem[43083] = 787
mem[55307] = 818610470
mem[13145] = 35815
mask = 00101X01001010X011111101X1001X010101
mem[34] = 928
mem[45041] = 12613
mem[20825] = 18260
mem[33068] = 34949
mem[51299] = 1228577
mask = X11X0001101X001X11010001110X00X01X11
mem[5587] = 3120
mem[49934] = 166656215
mem[12068] = 15929077
mem[51471] = 225039
mem[35781] = 703735
mem[2227] = 788305092
mem[30564] = 4758296
mask = 1111X1X000100X10X10010011XX011010110
mem[44917] = 195410918
mem[33352] = 480672
mem[18755] = 6376
mask = X1X1010000100010XXX0XXX011X011010000
mem[48717] = 6124
mem[6213] = 13466626
mem[33804] = 41450
mem[42319] = 1001042
mem[32039] = 22800
mem[37805] = 3719
mem[21650] = 5310058
mask = 1X110X0X00101X10110100111X10000X00X1
mem[64021] = 527213
mem[54049] = 2076239
mem[23361] = 10197597
mem[29280] = 28451017
mem[3136] = 46819
mem[21711] = 62402971
mask = 1X01000100X01110X101000101000XX10X01
mem[20911] = 1251
mem[3136] = 84
mem[27425] = 174128253
mem[20825] = 20910
mem[36929] = 13761934
mem[44528] = 983
mask = 1111X000001X001X11111XX1X00X0110000X
mem[516] = 6216446
mem[33582] = 140720
mem[11248] = 208955105
mem[42641] = 4364
mask = 101X1X10101X10001XXX0001111100011X11
mem[38531] = 39598
mem[46500] = 52608117
mem[43473] = 96876
mem[14304] = 59167889
mem[53240] = 15525
mask = X001100X00X01110111X1101010X00101111
mem[14918] = 2357
mem[6160] = 1208519
mem[34729] = 1611506
mem[51158] = 2346
mask = 111X00100000101011010111001110XXX1X0
mem[28673] = 331536
mem[43917] = 324
mem[45633] = 398602
mem[49398] = 1861
mem[1376] = 625904
mask = 01101X00001010100101100X101X10X00011
mem[28094] = 8805622
mem[30532] = 3723
mem[4708] = 14406
mem[9094] = 50623
mem[61707] = 73798
mem[4885] = 224
mask = 11X1000X001011X011010X01010XX10X0001
mem[7236] = 865782
mem[15953] = 487381549
mask = XX1011000010X11X010X11111100X11100XX
mem[18078] = 16242735
mem[5995] = 18850
mem[9604] = 839527
mem[47055] = 216488
mem[4059] = 554189
mem[12823] = 346
mask = 111100010010111X1X01X1110001X001X000
mem[936] = 6429561
mem[40513] = 1695095
mem[60924] = 252
mem[55142] = 557439
mask = 10111010X010X0X01XX1100111010X1X1X00
mem[38842] = 738
mem[30409] = 10094059
mem[303] = 98943
mem[8017] = 251227
mask = X1X0X00100100010X101001100110001X100
mem[57304] = 1975648
mem[56257] = 1580
mem[45571] = 14800
mask = 0XX00011001000X0X1111X11101X101X1000
mem[43189] = 4422
mem[40391] = 167654070
mask = 1110X01100100X10111XX01X1000XX01100X
mem[33804] = 1018
mem[29496] = 93900747
mem[9245] = 138506
mem[62151] = 674748
mem[4762] = 434956
mem[37596] = 103373
mem[27932] = 5730935
mask = 0111X0000010101X111X01001010101110X0
mem[36174] = 5162986
mem[33114] = 3116668
mem[64325] = 3171
mem[6917] = 5566099
mem[30076] = 176956
mem[14847] = 60238674
mask = 1110100X00100010XXX1X110000000001X01
mem[2546] = 7549354
mem[22803] = 386154
mem[28386] = 86719
mask = 11110100X00011X011X10X0011011010X100
mem[52611] = 131
mem[47628] = 266
mem[22198] = 764
mem[4444] = 406
mem[20006] = 3520555
mask = 10111X101010X000100110011100X01X1010
mem[15300] = 345854040
mem[49359] = 38235897
mem[38614] = 54370972
mem[18472] = 94915
mem[36746] = 697
mask = 11111011X011101X111X10100100000X0111
mem[29295] = 943139945
mem[9245] = 1505677
mem[14391] = 3398
mask = 1001X1X1X1001X10010100001X01XX11X101
mem[14613] = 15025
mem[58121] = 1912
mem[300] = 37310097
mask = X011001X001010XX11X1100X00XXX0011110
mem[17438] = 69372377
mem[39932] = 850
mem[53370] = 7579742
mem[49975] = 2831104
mem[37777] = 2859
mem[49238] = 508
mem[57733] = 3881
mask = 11110010001010X011X1X1X11XX00XX101X0
mem[29112] = 64162
mem[9687] = 10115647
mem[8456] = 404111433
mem[23461] = 21916957
mem[49272] = 29938
mem[20911] = 3016
mem[45508] = 27114
mask = X11010X000101X10X1X10X1XXX0110000111
mem[17094] = 1379888
mem[19615] = 633
mem[43148] = 125580
mem[12097] = 33116239
mem[34723] = 9358
mask = X111X010010X101011110X00011111000101
mem[63053] = 691425156
mem[24475] = 286162
mem[54863] = 76914
mem[58008] = 1422
mem[24141] = 657
mem[34729] = 49841
mask = 1110XXXXX0X011100X0101X1010X10001100
mem[14613] = 4510
mem[47926] = 39745
mem[8475] = 327407289
mem[22599] = 699202
mem[53880] = 822
mem[56755] = 1267566
mask = 1X1110X1101X100011X110X01X0001110XX1
mem[8272] = 301
mem[65364] = 3792396
mem[38614] = 591
mem[10929] = 4608
mem[30304] = 2186
mask = 1110000110X0001011010011110000X0X01X
mem[12247] = 1533938
mem[24376] = 12421
mem[16168] = 17660
mask = 001000000010101X11X101X00001X00XX0X0
mem[606] = 2029
mem[55307] = 598939
mem[26724] = 59403185
mask = 11X10XX000X01X101X01010110000X00000X
mem[4022] = 39755
mem[39281] = 801343
mem[46388] = 161
mem[21585] = 11357692
mem[45828] = 28056
mask = 11111X11101111001101XX11X1010X00X11X
mem[47755] = 3034
mem[42293] = 208825
mem[29700] = 38238
mem[22485] = 1319489
mask = 110100X0001X101X11010011100X000X0101
mem[3549] = 18335450
mem[58890] = 2676627
mem[58022] = 17558
mem[54581] = 2533100
mem[10929] = 280423870
mem[63555] = 2319
mem[2546] = 926
mask = 1110X011001XXX101111X1110001X110X001
mem[23536] = 4353
mem[59601] = 798347
mem[20671] = 389086
mask = 111100X000101010110101001XX1XX11X110
mem[1922] = 27551122
mem[19645] = 370473454
mem[32150] = 214932
mem[22144] = 2339
mask = 111000100110X01X1101101XX01111010X00
mem[35424] = 110774
mem[57733] = 531999
mem[45139] = 30278097
mem[21648] = 16240
mask = 1010X00100X00010111101001X0X00X10010
mem[58367] = 71950736
mem[855] = 21617593
mem[27425] = 1287663
mem[45595] = 1472271
mem[37043] = 1635315
mask = 111X00X1001X110011010010X0001100000X
mem[7355] = 208029905
mem[52551] = 6755
mem[49542] = 11911
mem[44588] = 377151
mem[64240] = 61021
mem[8017] = 3572
mask = 0X101000001011X011X101X0000X0000X011
mem[47482] = 1568110
mem[28053] = 707
mem[60195] = 1645
mask = 11110000001X101011X1101010110X00011X
mem[27869] = 17384703
mem[17643] = 2734129
mem[12802] = 3774302
mem[64154] = 916
mask = X111X00X0010110011010X01111001010XX1
mem[64313] = 102372
mem[1522] = 15753175
mem[40584] = 728098
mem[4315] = 227246865
mem[55272] = 7469
mem[15889] = 7541958
mem[2464] = 371279
mask = XX01X011X01011101101X00XX10X00110100
mem[5157] = 275
mem[12864] = 4756
mem[31054] = 1960
mem[52498] = 2797835
mem[62151] = 4302055
mask = 0X1X00X000101X1X1111010X000101X00110
mem[10919] = 612216
mem[15357] = 629148
mem[52500] = 9503
mem[17094] = 7855156
mem[6652] = 34492510
mem[10426] = 726153
mask = 010X00110010001X1111101XXX01X0X00101
mem[12974] = 9133
mem[14108] = 3766
mem[24429] = 1803969
mem[55142] = 36626772
mem[34019] = 473252
mem[33604] = 206855588
mask = XX1010X10010X01011X11X0011X01011XX11
mem[60027] = 841211
mem[21538] = 91609915
mem[6123] = 97596
mem[50346] = 1087
mem[40221] = 644
mem[8944] = 551344
mask = 111000X0001XX0100111111001100X00000X
mem[35547] = 950205
mem[3312] = 880
mem[48780] = 112556701
mem[4531] = 126242
mem[39889] = 1541
mem[24463] = 9010512
mask = 11XX101X1X101X001X0111X101000X1X0001
mem[64081] = 140604
mem[59066] = 420736
mem[48653] = 54226
mem[32039] = 128940
mem[35847] = 5
mask = 1001XX01X000111011X1X101000X0X0X1011
mem[58689] = 43170762
mem[12832] = 298144839
mem[1474] = 132921
mem[48057] = 37242
mem[50871] = 3602555
mem[56611] = 31214078
mask = 101X000100X011X01101XX10011X0XX1X011
mem[7317] = 2861082
mem[59424] = 214468
mask = 10X110111011101010110X10111001100XX1
mem[19313] = 2195784
mem[38614] = 1263318
mem[26637] = 54905795
mem[8165] = 209848
mem[2919] = 173
mem[928] = 7285
mask = 1011101XX011XX1X10111000011010X11111
mem[55792] = 11794686
mem[48675] = 377
mem[6484] = 857
mem[60951] = 523305753
mem[40488] = 713
mem[61858] = 12068
mask = 1110X1XX0010111X01011111X000X0X1011X
mem[65124] = 178147455
mem[15914] = 3503977
mem[35424] = 232628
mask = 11X0XXX100100010X1X101001XX100110001
mem[14344] = 2670
mem[20825] = 27980258
mem[2792] = 447228346
mem[29177] = 822367210
mem[30651] = 1658444
mem[14099] = 389
mask = 110001X1X010X110X101X011X100X1X10110
mem[54275] = 15249248
mem[3293] = 9475932
mem[8835] = 864371828
mem[45269] = 3632
mem[19645] = 580157
mask = 101X101XX01X10X01X1110X0110X1011011X
mem[40765] = 53494316
mem[35543] = 11261002
mem[13933] = 768
mem[38594] = 464498
mem[64240] = 22406
mask = 110100010X10111011X100X1010001X1X1X0
mem[20911] = 225
mem[12748] = 621
mem[29907] = 363502
mem[14613] = 3957288
mem[64313] = 389459
mem[21648] = 2008398
mask = 111X101X00X0X010X1110110X00011X1X011
mem[33134] = 1049991943
mem[25205] = 4126902
mask = 1X10101001101X1011X100X00X1X1101X100
mem[18755] = 713
mem[38650] = 119518
mem[33804] = 3211485
mem[14609] = 10752655
mem[59822] = 234204205
mask = 1X1XX0111X1X11001101100X110100X0X010
mem[44041] = 255408886
mem[62469] = 22090167
mem[12247] = 1760642
mask = 1110101000001X101X11010010XX110001XX
mem[30389] = 29125
mem[10833] = 1979
mask = 11111010111X1X001001111X1X1000010001
mem[20949] = 90971
mem[16774] = 9470584
mem[56713] = 73907316
mem[9264] = 36226
mem[60287] = 571
mem[25303] = 16376
mask = X1010X0XX01000X0001000X0X0010000X110
mem[59573] = 60076
mem[14516] = 1634251
mem[36459] = 843815961
mem[3293] = 265
mask = 01010100001X00101100X0X0111X11001101
mem[28480] = 437785448
mem[29724] = 28414900
mem[22867] = 14233982
mem[45019] = 1644931
mask = 111XX0100XX0101011X1011X001X11X101X0
mem[28545] = 461
mem[14609] = 3934
mem[50765] = 103069037
mem[12832] = 1926345
mem[36929] = 1633120
mem[33372] = 23156328
mask = X11X000000XX11X011X1XX0110000X000001
mem[12748] = 2069634
mem[35456] = 8101313
mem[21508] = 243298359
mask = X1X01001011X001001010110101100X10011
mem[27953] = 3450
mem[22680] = 50713870
mem[55768] = 182841527
mem[29021] = 70952
mem[59553] = 38886
mem[45283] = 84846018
mem[29849] = 66463
mask = 100XX1111X0011100X01000X010000000101
mem[53280] = 2165
mem[551] = 556704
mem[3512] = 16238
mem[2868] = 27158838
mem[23659] = 245809
mask = 1110X001X010X0101X01110100000X10111X
mem[36789] = 51664764
mem[33348] = 1689925
mem[19745] = 849985
mem[7335] = 3678
mem[25634] = 2206
mem[13782] = 1985
mask = 11110000X0100110XX11X0110100X0110001
mem[18372] = 193162
mem[5878] = 7411977
mem[30564] = 89127
mem[45210] = 64823202
mem[22016] = 3987667
mem[7092] = 370183817
mem[64313] = 2487681
mask = 10X0000100100X101X1X1101110000X0X011
mem[6160] = 9833
mem[26168] = 120117155
mem[13319] = 18180
mask = 1X01X001X0X01X1011110001100X0110X100
mem[29267] = 385
mem[13597] = 535380132
mem[58085] = 3408333
mem[46178] = 246464
mem[64848] = 140510
mem[19733] = 322520311
mask = 1101101110XX1000X001101110000000X1X0
mem[56355] = 894
mem[13795] = 13763
mem[38247] = 1412147
mem[46043] = 10668
mem[9370] = 8326048
mem[27470] = 524
mask = 111010010X1000100X01X1XX10XX0XX10000
mem[45269] = 129135
mem[33483] = 11595926
mem[9827] = 18572496
mem[27939] = 32963714
mem[48103] = 89693846
mask = X11XX111001001101101010101000X000101
mem[12666] = 61469
mem[40505] = 14463
mem[49277] = 118992
mem[16161] = 105
mem[23321] = 183700
mem[48653] = 930591910
mask = 111X00100X0X1010110100001X011X1X00X0
mem[59706] = 227615179
mem[21221] = 1424545
mem[54816] = 37682162
mem[57789] = 91718
mem[25191] = 3615219
mem[10390] = 240
mask = 1110X0XX001XX010X1110110001011010X01
mem[60974] = 5579328
mem[61131] = 337545
mem[16774] = 2030
mem[6637] = 5249")
