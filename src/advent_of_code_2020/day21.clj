(ns advent-of-code-2020.day21
  (:require [clojure.string :as s]
            [clojure.set :refer [difference intersection union]]))

(def ingredients-list-regex
  "Matches an ingredients list with allergens."
  #"([a-z]+(?:\s+[a-z]+)*)\s+\(contains\s+([a-z, ]+)\)")

(defn- parse-ingredients-list
  "Parses a single ingredients list into sets of ingredients and allergens."
  [s]
  (let [[_ ingredients allergens] (re-matches ingredients-list-regex s)]
    {:ingredients (set (s/split ingredients #"\s+"))
     :allergens (set (s/split allergens #"[\s,]+"))}))

(defn- parse-food-list
  "Parses multiple ingredients lists, one per line, into a sequence."
  [s]
  (->> s
       s/split-lines
       (map parse-ingredients-list)))

(defn- candidate-ingredients-for-allergen
  "Returns the set of ingredients which could possibly contain the given
  allergen."
  [food-list allergen]
  (->> food-list
       (filter #((% :allergens) allergen))
       (map :ingredients)
       (apply intersection)))

(defn- all-allergens
  "Returns the set of all allergens."
  [food-list]
  (->> food-list
       (map :allergens)
       (apply union)))

(defn- all-ingredients
  "Returns the set of all ingredients."
  [food-list]
  (->> food-list
       (map :ingredients)
       (apply union)))

(defn- safe-ingredients
  "Determines the set of ingredients which can't possibly contain any of the
  allergens in any food."
  [food-list]
  (->> food-list
       all-allergens
       (map (partial candidate-ingredients-for-allergen food-list))
       (apply union)
       (difference (all-ingredients food-list))))

(defn- number-of-foods-this-appears-in
  "Returns the number of foods the given ingredient appears in."
  [food-list ingredient]
  (->> food-list
       (filter #((% :ingredients) ingredient))
       count))

(defn solve-1
  "Returns the number of times any of the definitely safe ingredients appear in
  the food list."
  [s]
  (let [food-list (parse-food-list s)]
    (->> food-list
         safe-ingredients
         (map (partial number-of-foods-this-appears-in food-list))
         (apply +))))

(def trial-input "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)")

(def real-input "sklt ffvp zrbx kfff bkvs tsxf qrzzxr zmlfv btjt jxvr kqhdgl npccpr nmb zsrxjr zxhqz cjbnkl qhvz gdtfn bllq hjsffq jgnr gptv gbrz mfms pgllp rfm hplqjv dmdq dnxzfj lvzzx fzsl dmftng slc ghkpjc rzk zkljrvc pqxmx cvxk hmnklc bmj cdgm vrvsnf mksmf czsx fblv jtdn fzvxq sjtmns mjzrj mbgr ggmnz rmtjlx vgld tmk hzv nmrnmgcr xjtgf gjpcz vqmsh qbxht kpb blfbjjh kplzmn ktc lhmfqhc sjmjf qlpgl rvbr kgkrhg zclq ztfpgg bjdt zshfb chngq gns vlqpdm bdfs fzsdsn srkt mfkl vtxm xlctnvtx kscl frggd xztlr ggqc kqq (contains peanuts, soy, shellfish)
ktzzp lnqn lfrcrd srkt kgkrhg pgllp qhvz vmxb cdgm xztlr dmftng sklt dmdq ztfpgg tptvd mksmf bdfs cnnh llrnpqp pnb vzl gnkxpmx qfbnb gflr nmb vpqlj ghkpjc mmcm chngq nbgb ngfbtm bmj vgkkp gdtfn hqxn zsrxjr khgkdr rvbr scx pgxgv ppvhxt zmlfv hfxpz kpb thrdbhmv dnxzfj kqhdgl bbsm gns hpfb kbcpn ffpmj sjtmns fxnxp krbr tqstjfdc cbkn lvzzx hmnklc thfvrj mjzrj hplqjv fzvxq blfbjjh rbgqhb fzsl dbkjjhg rgqc vjsvx sjmjf (contains shellfish)
qfbnb khgkdr bljp bdtmh kgkrhg kbcpn vqmsh cdgm vbh gns tclpv ssj fzsl jgnr thfvrj scx vggchc mjzrj gnkxpmx bmj dlnht xdbn vrvsnf fxnxp rdmh gbrz jrcfmg kqhdgl llrnpqp khktc zshfb gptv xmtspv mnv hfxpz ggmnz rvbr cjbnkl ffvp khkk kpb hqxvz mksmf ktkz vgkkp btjt gjpcz lnqn bdfs fzsdsn kplzmn jtdn cbmt ffpmj bxrbp nmrnmgcr szvbsh kdq lhmfqhc chngq tmk qlpgl ggqc jrl gflr vlqpdm pnb fqxg srkt kqq rzk ztfpgg mmcm xlctnvtx ctzvf hplqjv lnkh tkl (contains nuts, soy, fish)
pbzl mfms zntmht ppvhxt lbjkrx gflr vqzzn mjzrj cvxk zxhqz bjdt hpfb jtdn ktkz ddnr zsrxjr dmdq tnxq thrdbhmv qbxht gptv ggqc pgllp khktc qhlzhd vzl rckkd vgkkp frggd fzsl mdlkr xztlr xbzp pqxmx mksmf lvzzx lnkh ggmnz xrxfjb nmrnmgcr szvbsh gjjk kgkrhg rfhc slc rvzst kbcpn mrdjsl tmqmbf slgfnp lhmfqhc qhvz gjpcz kpb vmxb hfxpz zclq lnssx zkljrvc qcrq rbgqhb mprhv bsfddz fblv bdtmh xzmj ffvp vqmsh vndmgdn (contains peanuts)
vgkkp sklt jgnr zvrjxt pgdxn mjzrj bkvs blfbjjh gptv xbzp jtdn kqhdgl bjdt bmj kgkrhg nqfrl lfrcrd lnqn khktc rbgqhb bljp fzvxq vgld mvxslbq fzsl hqxvz xlctnvtx jrcfmg lbjkrx njck pjtf jxvr cnnh ggqc dmftng thfvrj tmqmbf zclq kbcpn hpfb qqnfsjh nbgb kfff zshfb ffvp xrxfjb vzl mksmf (contains shellfish)
jrcfmg ctzvf rgqc qhlzhd bmj pgllp nmb ggmnz tptvd khkk gjpcz nqfrl ffvp qqnfsjh kgkrhg gptv mksmf bbsm mlzr blfxjj hfxpz blts sjtmns rfhc sjmjf mnv kbcpn kscl bsfddz lvzzx fzsl mdlkr lbjkrx mrdjsl qfbnb gdtfn bdfs slc llrnpqp cbkn kplzmn xdbn fzsdsn vrvsnf vgld nmrnmgcr cdgm xztlr kqshvlf cjbnkl tmk xlctnvtx tsxf zkljrvc qhvz lfrcrd rfm pppr lhmfqhc (contains shellfish)
ddnr kqhdgl xmtspv xbzp fblv czsx ppvhxt gptv ggmnz cdgm xjtgf tmk rdmh zkljrvc szvbsh qlpgl mlzr jrl kgkrhg qqnfsjh qfbnb kdxkkz fzvxq zvrjxt bljp pgdxn dmftng rzk mksmf bdtmh mbgr nmb xdbn mprhv vgkkp hlhzmqv xztlr gjpcz kdq vlqpdm chngq gjjk thfvrj kscl ghkpjc pqxmx bkvs zsrxjr scx tkl rdbt dlnht kbcpn qhvz tnxq rmtjlx gnkxpmx npccpr zmlfv njck bmj rbgqhb qrzzxr cjtsfvl bjdt ffvp mdvrb hplqjv mjzrj (contains sesame)
cdgm vqmsh mvbmxn hfxpz kqhdgl nmb bjdt scx tnxq bllq gptv dmftng mlzr vgld qbxht szvbsh kgkrhg khkk khgkdr ssj qhvz mksmf gjjk kqshvlf blts hlhzmqv bljp gns fxnxp dnxzfj ktzzp ghkpjc lfrcrd vrqf blfxjj lnkh mfms nsznhl mdlkr fzsdsn rfhc mfkl zxhqz vggchc tmqmbf bmj dmdq mjzrj jgnr hplqjv gdtfn sjtmns fzvxq chngq pqxmx kbcpn xlctnvtx khlr (contains fish, sesame, shellfish)
kpb qlpgl lnkh mnv vrvsnf vgkkp ssj sklt btjt tmk rgqc pgllp qhvz rmtjlx vggchc qhlzhd bjdt thrdbhmv kbcpn kgkrhg dmftng bmj kqq jrcfmg zkljrvc lbjkrx vrqf mjzrj bsfddz czsx fzsl zclq nbgb sjtmns mfkl qfbnb zntmht tqstjfdc vqzzn kqhdgl cnnh hqxn gns gjjk scx mlzr kdq zrbx jtdn tnxq zshfb rvbr xztlr ghkpjc npccpr njck mksmf ggmnz pnb pppr mfms nmrnmgcr blts hpfb fblv sjmjf vzl cjbnkl dlnht kplzmn kdxkkz (contains nuts)
rckkd kgkrhg mvxslbq mlzr ggmnz rfhc mnv rvbr ddnr bdfs cjbnkl cx kbcpn rzk ffpmj jrl blfxjj vndmgdn hjsffq zntmht dlnht vqmsh qcrq dmdq cdgm fzsl bbsm bjdt xjtgf tkl tsxf vqzzn xdbn qfbnb vbh pjtf qhlzhd dnxzfj qhvz slc qrzzxr szvbsh pppr nsznhl scx njck mjzrj kbjdrvk bsfddz bljp gptv frggd zmlfv mdvrb nmrnmgcr btjt vjsvx fxnxp mvbmxn vrvsnf bdtmh zxhqz kscl blts sjtmns chngq tqstjfdc sklt mrdjsl xztlr hplqjv fqxg kdq zsrxjr gbrz blpj rfm mksmf xzmj pgllp (contains wheat)
npccpr zsrxjr blfbjjh kbcpn gptv tsxf bmj mjzrj qlpgl pjtf rckkd gjjk tqstjfdc fkpnll zrbx rdbt krbr frggd mfkl kfff dlnht xmtspv qrzzxr rfm vgld sjtmns rdmh kdxkkz kgkrhg btjt njck vbh bsfddz mbgr blfxjj kscl cbkn lnkh mvxslbq mksmf kqq dbkjjhg cjbnkl fqxg gbrz bkvs vpqlj hzv pgxgv ppvhxt lnssx slc slgfnp khkk rvzst nmb fzsl ggqc blts lvzzx vqmsh qfbnb (contains peanuts, soy)
xzmj hplqjv fzsl hlhzmqv jrcfmg tsxf npccpr hfxpz zkljrvc gns ngfbtm qhvz pgdxn gjjk qlpgl sklt vgkkp xdbn khgkdr qqnfsjh rdmh fkpnll cjtsfvl mksmf pppr mjzrj xlctnvtx kbcpn slc ppvhxt khkk fjqdxd vpqlj fvmstz ffpmj kbjdrvk vrvsnf rgqc qrzzxr zshfb jrl mnv bmj gdtfn vzl lnssx bxrbp dbkjjhg rbgqhb vqzzn rzk ghkpjc zmlfv khktc pqxmx fblv zxhqz zvrjxt fxnxp pbzl krbr gptv (contains sesame)
qhvz rfm lhmfqhc vgkkp hpfb frggd cbmt khktc slc cx qlpgl vpqlj kdxkkz scx ssj srkt cjtsfvl vqmsh mlzr szvbsh xmtspv njck zkljrvc ffpmj hqxn cvxk bdtmh ktkz nqfrl blfbjjh mfms mjzrj bmj ggmnz gns bkvs rgqc kbcpn ctzvf dnxzfj gjjk jrcfmg lfrcrd vrqf gptv kgkrhg mfkl pbzl fxnxp mbgr mksmf (contains soy)
mnv qhvz nqfrl pgdxn blpj hzv cjbnkl zrbx bdfs gflr vlqpdm gjjk lnkh gdtfn qcrq lnssx frggd cjtsfvl ctzvf bjdt hqxn thfvrj chngq xrxfjb vggchc kgkrhg bljp qbxht dnxzfj hfxpz mrdjsl czsx llrnpqp vgld bbsm xjtgf qhlzhd ghkpjc bmj pnb pjtf xlctnvtx ktc slc bdtmh khlr zmlfv bkvs gptv dmdq vzl nmrnmgcr dbkjjhg hmnklc vbh mjzrj hlhzmqv gbrz khktc mdlkr kbcpn jtdn pgllp mksmf tsxf qrzzxr vndmgdn vmxb hpfb tptvd fvmstz ggqc rzk xzmj zntmht kfff kbjdrvk kqshvlf mdvrb vpqlj (contains soy, peanuts, eggs)
hpfb tsxf qcrq qvjsd khgkdr jgnr qfbnb sklt rdmh kplzmn bjdt mprhv rzk lnkh fxnxp zvrjxt cbkn xztlr xjtgf jxvr scx vndmgdn pgdxn lbjkrx nbgb srkt xdbn qrzzxr hplqjv khktc nsznhl jrl gflr szvbsh qqnfsjh gnkxpmx hlhzmqv pqxmx mjzrj zclq zkljrvc qhvz mvbmxn pnb ddnr rdbt fqxg tptvd ngfbtm vtxm ggmnz bljp fkpnll kqq ffvp lfrcrd fzsl kgkrhg gjpcz hmnklc kbcpn rckkd gptv vjsvx chngq mksmf njck ggqc mfkl zsrxjr vqmsh fzsdsn blpj ghkpjc blfbjjh (contains soy, nuts, eggs)
rfm fzsl ztfpgg gjjk lhmfqhc blpj cnnh sjtmns thfvrj jrl hfxpz jrcfmg vgld vbh rmtjlx btjt gbrz blts ktzzp tmk rdbt cx fzvxq khlr ngfbtm zclq kbcpn bkvs xjtgf kgkrhg vndmgdn mksmf czsx zntmht lvzzx llrnpqp gjpcz srkt zmlfv xbzp cbmt pgdxn njck xrxfjb qhvz dmdq tkl fqxg bbsm mjzrj bmj kqq qrzzxr (contains wheat, eggs, sesame)
tnxq hjsffq jxvr lvzzx krbr pnb qbxht rgqc jrcfmg gns mdlkr gptv bljp sjtmns cjbnkl jgnr bmj ggqc gflr mfms fzsl dmdq bdtmh rdbt rckkd frggd fzsdsn hmnklc lnqn pppr qvjsd qhvz jrl xrxfjb mbgr sjmjf xlctnvtx kgkrhg rvbr bdfs vpqlj vmxb khkk qrzzxr vlqpdm vtxm hpfb btjt kqq qlpgl nbgb kbcpn vrqf mksmf bsfddz (contains nuts, shellfish)
zmlfv ggmnz vqmsh krbr tmk gptv vndmgdn ghkpjc vpqlj ktzzp zshfb czsx qlpgl xzmj fjqdxd rgqc kbcpn jgnr zntmht njck kplzmn ddnr tmqmbf kgkrhg bbsm fzsdsn llrnpqp pnb nmb tptvd zkljrvc kdq fzsl pgdxn dnxzfj qcrq szvbsh qqnfsjh bllq vrqf qhvz cvxk xjtgf frggd mmcm mvbmxn sjtmns thrdbhmv bmj zvrjxt mjzrj mbgr fblv xbzp bjdt ffvp srkt fkpnll rvbr nbgb lnssx jrcfmg (contains sesame, nuts)
ggmnz zrbx scx mvbmxn zmlfv qlpgl fzsl vqmsh tnxq blts gbrz czsx vjsvx xzmj hqxn tkl lfrcrd qfbnb kqq nbgb zsrxjr qqnfsjh kgkrhg kbcpn szvbsh vndmgdn lnssx pbzl lbjkrx vtxm hlhzmqv mksmf qbxht xmtspv bljp gptv xlctnvtx hqxvz cx gns bmj xjtgf lhmfqhc lnqn ktzzp mjzrj zclq bdtmh qhlzhd lnkh fzsdsn tmk sjtmns pgdxn rzk (contains peanuts, wheat, fish)
mrdjsl mjzrj qvjsd khlr nmb hqxvz pgxgv kpb gjjk frggd ktc gdtfn pgdxn cbkn mvbmxn nsznhl njck pqxmx mnv krbr mdvrb jgnr kdxkkz mprhv lnssx thrdbhmv qhvz blpj hjsffq thfvrj xlctnvtx mmcm lvzzx rdmh kbcpn gflr blfbjjh mfms rckkd rfhc fzvxq jtdn bsfddz fzsdsn blts dmdq qqnfsjh xmtspv rfm vmxb khgkdr gnkxpmx kgkrhg nqfrl ssj gbrz bmj sklt rdbt hfxpz gptv mksmf bllq ffvp tclpv ktkz xjtgf bxrbp bbsm kqhdgl zclq ngfbtm vgld kqshvlf fjqdxd hplqjv scx fkpnll dmftng lfrcrd qlpgl lnqn zmlfv xzmj tptvd rbgqhb kplzmn pgllp bljp bjdt slc vzl vlqpdm vndmgdn hmnklc zsrxjr ghkpjc mfkl (contains peanuts, shellfish)
bxrbp vgkkp kbcpn zrbx nqfrl kgkrhg lnkh frggd xdbn czsx mvxslbq gnkxpmx cdgm mjzrj fxnxp kdq jtdn mksmf jxvr pbzl zxhqz bllq bsfddz bmj fzsl pjtf btjt gbrz ngfbtm mfkl sjmjf pgllp cbmt ggmnz vzl ppvhxt njck vrqf qhvz fvmstz slgfnp sklt tmqmbf dmdq sjtmns qbxht kdxkkz fzsdsn dbkjjhg rzk vbh hqxvz fjqdxd cvxk rmtjlx lbjkrx blts gjpcz ktc zntmht (contains shellfish, nuts)
ktkz khktc mfkl mksmf mvbmxn thfvrj nmrnmgcr vrvsnf hpfb dmdq bljp pjtf hplqjv ffvp qhlzhd tsxf bmj dnxzfj rvbr pgxgv gptv qlpgl ggqc xztlr gbrz cjbnkl fxnxp gjpcz rfhc xjtgf qhvz mfms lnssx qvjsd ktzzp bjdt cbkn llrnpqp gjjk ssj mnv blts fjqdxd gnkxpmx khkk vggchc thrdbhmv kqq ppvhxt tmqmbf njck blfxjj kgkrhg nsznhl bdfs qbxht kqhdgl cnnh mprhv slgfnp nmb qqnfsjh bsfddz dbkjjhg kbcpn kdxkkz lvzzx fzsl bxrbp vgkkp pnb kdq rbgqhb hlhzmqv rzk cvxk chngq sjtmns npccpr jtdn zkljrvc (contains wheat, sesame, nuts)
vrqf bbsm bsfddz rfhc rvzst xbzp bljp vtxm vpqlj lvzzx ktzzp zshfb rgqc blfbjjh bllq rdmh tsxf lnkh pqxmx vgkkp srkt cnnh ngfbtm rvbr tqstjfdc qrzzxr ffvp xdbn fzsdsn kbjdrvk vggchc cdgm vgld ctzvf npccpr gdtfn gjpcz tmk scx vbh xzmj zmlfv nsznhl rzk lbjkrx dmdq qlpgl hlhzmqv qhvz slc tkl fjqdxd hfxpz cvxk hplqjv mksmf fxnxp sjmjf fzsl szvbsh pgllp tclpv bmj bdtmh lnqn mjzrj bkvs ktkz kgkrhg qfbnb bdfs gns vzl zvrjxt gptv vmxb rdbt nmrnmgcr rfm pbzl (contains wheat, eggs, sesame)
chngq fvmstz gjpcz ktc pbzl gjjk qhlzhd dmdq zvrjxt pjtf ggqc rvbr cdgm rvzst rdbt bxrbp kdq xmtspv vmxb bmj khgkdr fjqdxd xjtgf lfrcrd khlr qhvz pgdxn vzl kdxkkz zntmht bdtmh cnnh jtdn gflr fkpnll mjzrj kpb pppr fxnxp hfxpz kplzmn ztfpgg qrzzxr gptv vtxm kqq vpqlj xztlr hplqjv mdvrb jgnr ngfbtm vjsvx fzsl mvxslbq hzv kbcpn blts lnssx sklt krbr ddnr xdbn xbzp czsx qbxht zsrxjr pgxgv zshfb kgkrhg nbgb rgqc slc dnxzfj bsfddz (contains eggs, sesame, wheat)
qfbnb krbr zxhqz rckkd fzsl ffpmj xdbn scx xbzp bmj cjbnkl khkk mbgr pgxgv cbmt bjdt gptv pjtf vlqpdm mjzrj sjtmns bsfddz fvmstz pppr gdtfn hmnklc btjt rdbt chngq thrdbhmv dmdq vbh kgkrhg dmftng qlpgl zkljrvc vqmsh blfxjj cjtsfvl kbcpn nmb xjtgf kqhdgl vmxb pbzl dlnht vgld xmtspv mksmf mvbmxn (contains wheat, shellfish, soy)
tmk mjzrj kdq srkt vggchc rckkd mfms jgnr pnb ztfpgg kgkrhg mvbmxn lvzzx lhmfqhc lfrcrd bdtmh tptvd mksmf mmcm zntmht zrbx ktc fvmstz bdfs sklt bmj lnkh vndmgdn vjsvx mnv ffvp xjtgf hjsffq rzk nqfrl mdvrb gflr btjt qlpgl fjqdxd qrzzxr krbr kbcpn mbgr fblv rbgqhb tmqmbf fzsl gns fkpnll mprhv rfm ssj hzv rvzst cbmt gptv (contains wheat, shellfish)
zshfb srkt njck ggmnz ghkpjc npccpr tmk mvbmxn mvxslbq llrnpqp nmb mmcm ffvp pqxmx jxvr gns fqxg ktzzp bxrbp zmlfv blfbjjh thfvrj pppr qvjsd lvzzx blts vndmgdn vqzzn tsxf bdtmh fzvxq xbzp tmqmbf xjtgf mksmf pbzl bsfddz tkl gflr hplqjv nbgb qhvz ngfbtm ssj sjmjf mjzrj qfbnb bbsm vmxb fzsl krbr kgkrhg jgnr cbmt mdvrb kqhdgl blfxjj mbgr zxhqz gptv vlqpdm gnkxpmx kbcpn mfkl nmrnmgcr rgqc rdmh dbkjjhg rckkd slc khkk fzsdsn gjjk rdbt jtdn rvzst hpfb bdfs (contains fish, eggs)
rckkd bmj ctzvf vrvsnf lfrcrd gnkxpmx kdxkkz gptv cvxk bkvs blpj rgqc rdbt nbgb bjdt rfhc mdlkr kbcpn vmxb vgld zntmht slgfnp qhlzhd kdq dlnht mlzr rvbr sjmjf njck ddnr kgkrhg dnxzfj qfbnb gjjk rmtjlx vbh vqzzn jxvr tnxq tmk ffpmj fjqdxd mksmf tmqmbf kbjdrvk gbrz vqmsh kqq mvxslbq nsznhl tkl fzsl pjtf mjzrj zxhqz dmftng (contains fish, soy)
khkk hpfb mbgr bdfs gbrz ffvp fzsl blfbjjh srkt dmftng vjsvx zshfb tkl slgfnp dlnht llrnpqp cjtsfvl npccpr pjtf tmqmbf nmb nbgb gptv tclpv rdmh dnxzfj ggqc qhvz bdtmh zxhqz blpj bmj mjzrj vrvsnf kbcpn pnb fqxg mdvrb jrcfmg fvmstz blts kgkrhg pppr jxvr scx rzk hmnklc kdxkkz ppvhxt rckkd vtxm ffpmj jtdn zvrjxt fzsdsn njck qcrq fzvxq xztlr lbjkrx kqq cjbnkl dmdq sjmjf (contains nuts, wheat, eggs)
bsfddz qhvz qlpgl zxhqz gbrz pgllp cvxk qqnfsjh tqstjfdc ddnr ktzzp cdgm qfbnb kgkrhg rckkd fzsdsn kbjdrvk bkvs blfbjjh cbkn pgdxn mjzrj ggqc kqshvlf chngq mfms gflr frggd kbcpn blts pbzl kpb rvbr bxrbp thrdbhmv khgkdr jrl hplqjv mmcm vggchc vbh fjqdxd gns bdfs rgqc tnxq scx nqfrl nmb tmk vgkkp krbr btjt bmj njck hlhzmqv jtdn xlctnvtx mprhv hpfb gptv dmftng vrvsnf vtxm cx kqhdgl mksmf (contains soy, shellfish, wheat)
xlctnvtx khlr kdq xmtspv nmb mdlkr sklt qlpgl vgld zclq krbr pnb mjzrj fzvxq tqstjfdc pbzl blts mbgr mprhv rdbt nsznhl btjt mksmf cjbnkl vpqlj bmj sjtmns cbmt pgdxn xrxfjb slc gdtfn gjpcz kbcpn vtxm njck qhvz bjdt fvmstz vggchc dnxzfj zrbx hpfb mdvrb jxvr bdfs bsfddz jtdn nmrnmgcr kgkrhg llrnpqp chngq fxnxp kscl gns bxrbp gptv kplzmn rfhc mrdjsl (contains shellfish)
hjsffq ktkz bmj ddnr sjmjf zclq mprhv frggd vpqlj xrxfjb kbjdrvk mvbmxn fzsl ffvp ppvhxt lnqn vtxm pgdxn kdxkkz srkt tqstjfdc ngfbtm qhvz mksmf kgkrhg rvzst bsfddz qfbnb ktzzp vgld blpj bllq khlr zsrxjr cdgm zvrjxt jrcfmg gptv tptvd pbzl rfm vlqpdm bljp mfkl fzvxq mjzrj fqxg fjqdxd slc tmqmbf nmb blfxjj (contains shellfish, soy, eggs)
lnssx gbrz ktzzp vjsvx rvzst mksmf kgkrhg vqmsh vtxm ktkz bdfs gnkxpmx pjtf ssj zshfb npccpr tqstjfdc scx nmrnmgcr kbcpn xbzp kbjdrvk pgdxn lnqn llrnpqp btjt tkl blts khkk rdmh qcrq nsznhl vgld ktc ggmnz cvxk cdgm vzl dmdq pnb mjzrj zclq pgxgv zrbx jtdn rckkd kplzmn bmj ghkpjc cjtsfvl jxvr zxhqz cjbnkl fzsdsn vmxb bjdt nbgb gptv tclpv vgkkp fzvxq njck sjtmns hpfb fzsl lvzzx fjqdxd nqfrl kqq (contains wheat, shellfish, soy)
cbmt vjsvx lnssx bdfs blts pgxgv krbr nqfrl fqxg zsrxjr mbgr ztfpgg fzsl cx rzk njck hqxvz kqshvlf bmj xdbn nsznhl rvbr vtxm qqnfsjh fblv mjzrj nbgb frggd blpj kplzmn rbgqhb kgkrhg rmtjlx ktkz khlr qhvz fzsdsn mlzr gptv dmdq cbkn ssj tmqmbf gnkxpmx xzmj cdgm zxhqz kpb khkk tmk tclpv ngfbtm ktzzp tsxf bllq kbcpn hpfb mnv llrnpqp slgfnp vbh hqxn dlnht gbrz gflr qhlzhd vmxb zntmht hlhzmqv cjbnkl (contains nuts, peanuts, soy)
rmtjlx dmftng tptvd ktzzp blts jxvr vrqf mjzrj ngfbtm kgkrhg rfhc kbcpn jgnr ggqc gns mmcm kqshvlf qvjsd kscl bmj qcrq hqxvz vlqpdm nmb vndmgdn bllq pgdxn hmnklc qhvz slc ktc mrdjsl rgqc gptv ppvhxt hpfb srkt mksmf kbjdrvk cjbnkl bkvs qfbnb nqfrl vpqlj mnv zntmht rbgqhb fjqdxd vgkkp sjtmns xrxfjb dlnht sjmjf krbr tkl gdtfn (contains peanuts)
mvxslbq ddnr czsx vgkkp kgkrhg bdfs mksmf vzl szvbsh nmrnmgcr gns xjtgf qlpgl sjmjf zmlfv bdtmh chngq ffpmj khkk fzsl gjjk ggqc lnssx qhlzhd fkpnll hlhzmqv nqfrl gjpcz krbr kqhdgl qhvz thfvrj kdq bljp slgfnp mjzrj ngfbtm dlnht gptv mmcm xzmj rdmh vbh ktkz mprhv jgnr pppr mbgr rgqc vggchc kbcpn kscl cbmt rmtjlx vjsvx vqzzn cnnh vgld dnxzfj tnxq hqxvz (contains nuts)
tqstjfdc gbrz ztfpgg pjtf zntmht pppr tmqmbf ctzvf lfrcrd ktkz hjsffq czsx pnb vlqpdm jgnr kbcpn kscl mdvrb rmtjlx slgfnp kgkrhg pgdxn kdxkkz mksmf pqxmx khlr ffvp srkt mjzrj npccpr zrbx vmxb jtdn vgld vqzzn bdfs cjbnkl fqxg bsfddz mdlkr llrnpqp hzv lbjkrx gnkxpmx rzk ktzzp rfhc xmtspv blts vggchc rckkd fjqdxd fzsl sjmjf lnkh fkpnll vrqf rdmh fvmstz qhvz xlctnvtx gptv bkvs nmb (contains soy, fish)
kscl blpj pppr rdmh mksmf vpqlj fzsl cnnh nmb xztlr rmtjlx kbcpn qlpgl tqstjfdc ctzvf mmcm ggmnz cjtsfvl zntmht hfxpz lnkh gjjk gptv rgqc sjtmns hjsffq ddnr chngq cvxk jrcfmg bllq qhvz jtdn hlhzmqv vbh dlnht pjtf kqhdgl zshfb btjt hpfb zkljrvc xjtgf kfff khlr vndmgdn vmxb mvxslbq czsx vrqf tptvd ztfpgg qhlzhd kdq bsfddz zsrxjr dmftng lvzzx rfhc zxhqz zvrjxt rckkd xlctnvtx tkl vtxm hmnklc bkvs cjbnkl fkpnll bmj srkt kgkrhg bljp jrl (contains eggs, nuts, shellfish)
kpb llrnpqp pbzl bkvs tptvd cjtsfvl kfff fqxg kdxkkz khkk ggmnz cbmt gptv nqfrl slc ffpmj gnkxpmx cvxk mjzrj rfm lnkh lhmfqhc dbkjjhg cx lbjkrx nbgb frggd xbzp vpqlj bbsm slgfnp hqxn fzsl qcrq mvxslbq mrdjsl gjpcz bdfs vrqf kgkrhg sjtmns tmqmbf tmk szvbsh hqxvz kbcpn npccpr zmlfv sjmjf ggqc scx vtxm mksmf hplqjv vqmsh pqxmx dnxzfj rmtjlx jrl qbxht dmftng qhvz srkt tqstjfdc zkljrvc bsfddz kplzmn fkpnll blfbjjh xztlr btjt kbjdrvk cdgm ktkz bjdt bllq xdbn khgkdr qqnfsjh jtdn (contains soy, eggs)
kqhdgl gdtfn pjtf jtdn xmtspv njck khktc sjtmns ssj nsznhl rdbt mprhv rvzst rmtjlx zshfb lhmfqhc cx vqmsh szvbsh qhvz mrdjsl xztlr blts vgld xzmj fzsl nmrnmgcr pgdxn kbjdrvk zrbx gjpcz mksmf vtxm vggchc pgxgv bmj bjdt blpj qcrq ktkz xdbn bllq ctzvf kpb thfvrj scx bxrbp hjsffq qqnfsjh mjzrj lbjkrx btjt pppr fvmstz tsxf rfhc xlctnvtx tmk kbcpn zntmht rdmh czsx llrnpqp mbgr cdgm mdlkr rgqc zsrxjr vgkkp kgkrhg mlzr tqstjfdc hqxvz mmcm (contains eggs, wheat)
qhlzhd qhvz mbgr ggmnz bllq xzmj czsx mjzrj jgnr tptvd pgdxn ztfpgg pjtf rzk dmdq kdxkkz bmj lfrcrd mksmf mdlkr lvzzx slgfnp hpfb hjsffq jrl ghkpjc gptv dbkjjhg bkvs nmb blfxjj kqq fqxg btjt rckkd blpj vpqlj npccpr nbgb cdgm gbrz rbgqhb vlqpdm zmlfv dlnht vqmsh xjtgf zntmht thfvrj cnnh vqzzn kqhdgl sjmjf qcrq xmtspv fzsl hqxvz ngfbtm gjjk ddnr kbcpn (contains shellfish, sesame)
vggchc jgnr mprhv kdq tmk krbr pgxgv kgkrhg kbcpn hqxn npccpr mksmf jrl xrxfjb lnqn ghkpjc nsznhl mrdjsl vndmgdn fzsl xjtgf khgkdr bmj pnb bdtmh mfkl qhvz gptv tmqmbf cdgm srkt zkljrvc slgfnp mdvrb gns ggqc vbh xmtspv mvxslbq mfms vtxm hfxpz qbxht hplqjv kfff fjqdxd (contains soy)
kqq mjzrj scx vrqf hzv hjsffq kfff jtdn cjtsfvl ssj cdgm vqmsh mlzr mdvrb chngq vjsvx xztlr pnb czsx tkl fzsl kbcpn hfxpz qhvz slgfnp gflr ffpmj bmj zxhqz tsxf vgld thrdbhmv sjmjf qhlzhd pgxgv rfhc mksmf mdlkr hqxn vzl ppvhxt jxvr tmqmbf gdtfn bljp khlr ghkpjc gns blts kgkrhg (contains wheat)
lnssx khktc zxhqz qvjsd vmxb zmlfv mprhv qhvz ffvp kgkrhg qbxht ktkz bllq bjdt jxvr vrvsnf pgdxn mksmf gjpcz xztlr blfxjj hlhzmqv xbzp ktzzp mdvrb khlr rzk mjzrj kbjdrvk zrbx sjtmns gns qhlzhd vggchc gdtfn jrl cdgm npccpr ggmnz czsx kqq vndmgdn fzsl fblv pqxmx vtxm lvzzx qlpgl mfms jgnr tqstjfdc llrnpqp tsxf ddnr xzmj kbcpn qrzzxr bsfddz hqxn cnnh gptv bxrbp (contains peanuts, fish, nuts)
njck qhvz gjpcz tqstjfdc czsx vzl pppr ghkpjc jgnr blfbjjh xdbn ctzvf frggd slgfnp bmj ffvp fzsl vndmgdn rfhc kgkrhg fvmstz tmk zmlfv cvxk fqxg vrqf hqxvz zvrjxt cjbnkl fkpnll qhlzhd gptv pjtf chngq vjsvx kbcpn mjzrj vgld pbzl kscl (contains fish, peanuts)")
