import scala.collection.mutable.ListBuffer

object Day2 extends App {

  val rawData: String =
    "1-7 j: vrfjljjwbsv\n1-10 j: jjjjjjjjjjjj\n9-13 s: jfxssvtvssvsbx\n10-12 d: ddvddnmdnlvdddqdcqph\n11-12 b: bbbbbbbbbrbnb\n7-9 q: qqqqlqmqqq\n8-11 z: zzzzzzzpzzlzzzzszzzz\n12-13 l: llnlllllllllll\n3-5 f: ffffwff\n16-18 c: cccccccccccccccsccc\n3-4 g: fsgkgffbdgqbzdn\n7-10 m: mmrflrmmfm\n18-19 w: wwwwwdwwwwwwwwwwwww\n1-11 x: xvtcrlffxrxjlxbl\n3-6 s: slsdqpwn\n1-5 z: zzzzj\n11-15 m: mmmmcmmmmmlpmqm\n10-12 r: rrrrrrrrrrrh\n13-16 v: bzjvkrvxlnvwvclg\n2-3 d: dpdd\n7-18 d: zddddnpdddddcdddddd\n2-3 r: rrrrr\n8-15 f: fffffffsffffffff\n10-12 g: gtcmfblggngv\n10-11 l: llllllllllc\n12-14 n: nnnnnnnnnnnnnnnnnn\n10-13 q: qqqqdwkqqqqldqds\n10-11 r: rrrrrrrrrrr\n2-3 k: khsk\n8-9 r: srrrdrsrrbrrrr\n5-7 n: wqrtnlqjnn\n17-20 w: wwwswwwwwwwwwwwwgwxh\n5-6 t: tmztjbtst\n8-17 g: gxglgssgqzggpgggh\n15-18 v: vvvvvvvvvjvvvvxlvz\n1-4 c: lccvc\n1-5 q: qsqqfqqqqqqqqqqqd\n5-6 w: wxdqjfwkswfwwzt\n3-8 r: rwtkslmrx\n2-6 b: bbbbbbbbbbbbbbb\n3-4 m: mjmm\n9-11 s: ssszssssssl\n7-15 m: ktmclnsnxzprkdcr\n3-9 z: lzzzzzzzzzzz\n1-10 g: hgdhggggpj\n11-12 d: ddpdsndgddddb\n3-8 p: kjhpchxpxfpgf\n2-13 d: znwcbsdjgjrldpn\n3-5 f: fdcmf\n2-3 d: dxdddd\n1-3 v: rgvfrgptwkrv\n1-9 t: cwttttttt\n11-12 n: nnnnnnnnncbngn\n1-12 x: gdzgxdvhctkxfwq\n4-6 f: fclbnf\n12-15 j: jjjjjjjjjjjhjjj\n8-18 d: ddrdddfldjlrldbdkd\n1-12 n: hnhnnnnngqnnnnnns\n4-7 c: cxgcntzcktkbrcvgfbq\n6-7 c: cccccct\n7-10 w: mlwcwgwwbcbwggwwgwwn\n1-5 p: ppppp\n1-16 r: rrrrrrrrrrtprxrrr\n5-10 v: vvvvvvvvvvvv\n12-13 z: wtzzkzzzjzzzmqzzzm\n10-11 d: dddddddddjd\n3-6 j: jjjjjj\n1-7 x: wxxxxxvxxx\n2-5 t: tmttt\n2-5 l: djmbckfsscq\n7-8 l: llllllltl\n7-10 z: lzzzzkzmzwzzz\n5-6 j: jjtjhj\n9-11 l: lllltlllftlll\n17-18 j: jjjjjjjjjjjjjjjjxgj\n3-6 b: btpgbbbbdrbqn\n7-8 t: ttttttqwt\n12-15 c: pccmccsccccfcccc\n14-15 v: vnvvvvvcvvvvvnmv\n12-14 t: bttttttttttttntt\n18-19 v: vvvvvvvvvvvvvvvvvktv\n1-5 w: wwwwl\n5-7 j: jdlbdjmjjrkc\n6-7 s: qsssdgs\n5-7 g: jcrtgxgg\n6-10 l: lllllllllsl\n18-19 b: bbbbbbbbbkbbbbvjbzbb\n1-4 x: xxxglxl\n14-16 k: kkkkkkkkkkkkkkkkk\n1-5 x: zbjnxxglt\n16-19 v: vvvvvvvvvvbvvvvvvvl\n6-7 j: jjjjjmj\n5-10 w: gswvknnpmmbx\n1-4 b: bmqb\n4-5 k: kkkrkkl\n5-17 d: ddhjxmmddddhzddsrx\n2-19 n: nnbjqmvlhnlfwxghnln\n5-18 x: hjrqfdfrhzwjwsjxjn\n1-2 m: mmmxm\n7-8 b: bbbbbjbb\n3-6 r: rrrrrw\n4-6 w: lwgwwpkwww\n3-8 n: nnnnjnnnnnd\n2-11 n: nnnvsnmnvqnbl\n15-17 x: xxxznxxxxxxxxxjxk\n13-14 d: ddddqdfxdddddddddgd\n14-15 v: vvvvvvvvvvvvvwv\n7-8 x: xdxxxxxx\n1-3 q: qkhqq\n3-4 v: jxvmpkp\n8-12 g: ggsfgggzggjggg\n14-16 k: lkzgkkkkbkkkgkkkk\n14-17 t: ttvtvtttttttcxfttttt\n7-11 r: dwlrbrzrrrrjr\n7-11 k: xkkzkpknknpkkzkzss\n1-7 c: cpkxbtccxzrcpdl\n11-13 w: wwwwwwwwwwwww\n1-11 d: wdddddddfbdxhdfdd\n5-9 d: djdhhjvjddddnjddsxqz\n13-20 t: ttttttttttstrttttttt\n9-11 l: qllkvnllhllllplz\n2-4 m: mmxms\n18-19 l: lllllllllllllklllkr\n8-9 n: nnnnnnnhw\n2-8 s: thsstssswssssss\n9-17 n: nncsnnnndnnptnnnvhnn\n6-8 r: rsnlrhrgsxrr\n2-3 n: nntb\n9-15 q: qqqqlgqqgqqvqqrq\n7-8 l: llllvslv\n10-13 r: rrrrrrrrrnlrd\n3-4 k: kskwjvbcchtk\n1-7 h: hqfgwhvshtqsgjjml\n2-18 g: wzbxrzqshkdptwvjsg\n5-7 n: nqnnmnn\n2-18 g: ggtgbzgggggggggcgggg\n2-4 c: ccgcwclcc\n4-8 v: qjvfvmkvvtvh\n2-4 d: lbcchdcbtlrhw\n15-18 l: ltvlrzqlwlwlpvllll\n1-5 p: mpvbfjpj\n14-19 h: hhfhhhhhhhhhdshgvhh\n5-8 p: ppphpqcrpwdxpxpfrppf\n16-17 d: ddddddddddddddddd\n11-12 n: nnnnnnnnnndp\n10-11 f: wfflfwfffrfblsgfvrff\n16-17 x: xxxxxxxxxxxxxxxfx\n2-4 k: kkckk\n3-5 g: tpggzglgzw\n10-14 v: vvvvhqtbvmvvvzn\n8-11 z: zzzzbtczzfvgzz\n4-5 l: dsvlllgldllcb\n6-7 b: bkbzbsb\n2-5 m: xrmmmmmmmmdmmmmmm\n3-4 f: fdft\n2-3 j: jjpjl\n5-8 g: ggjqmrgngdgmbz\n4-6 x: xnxwzk\n11-17 h: hhhhhhhhdhhhhhhhfhhh\n5-13 f: fffffffffffffff\n3-6 f: fvtxffff\n1-3 m: sjmmm\n15-16 s: ssssspssssssxssdsgs\n4-8 f: ffkfbtsw\n16-17 m: nhdqmnbqfkppbtmmmjx\n8-9 b: bbbbbwbnbb\n4-6 x: xxbxmx\n3-13 r: glqxrpkmjwbnr\n10-16 k: dkkqkkkgrklrdgtk\n5-10 r: hjrrrrldrwrdvrrrr\n11-12 l: cglcgxvlrrml\n6-12 r: rrqrrrrfkrrgrrtrxz\n4-11 b: bgvblbbtbnbbp\n4-7 j: pjxlbjcjk\n9-16 m: mmmmmmmmwmmmmmmm\n3-8 g: wxzpbpdgznzgsgdqgpp\n1-2 l: lwclllkldl\n12-15 r: rrgdvrrtqrrqrrrfrr\n6-7 l: lrljwzl\n5-12 m: mtmmbmmmmmmnmm\n14-15 x: xxxxxxxrxpxwdxzxxxxx\n3-5 k: kjkgtr\n1-5 x: xxxxl\n1-6 c: lcfzghdmlrlcdf\n4-5 p: pzpps\n12-19 t: mttlttttthktttttttl\n2-5 p: mpspppppppp\n2-3 d: dddpxtb\n11-12 f: hlbffffpkfffffzfffzf\n6-7 s: lskssssjqsg\n1-4 q: qnqqbqpmq\n17-18 t: ptttttttttttttttsmt\n3-4 x: xnmx\n1-4 s: ssssbh\n6-16 b: bbbbbcbbbbbbbbbfb\n1-14 f: fwccgsfxljvzwbls\n2-3 b: bbfk\n5-9 d: ddndxdddfd\n5-10 d: ddcddddddd\n9-14 b: gpbbqbbbcqbbbf\n10-12 l: wlllllqllllllll\n1-8 n: nvwdffhbdlkv\n13-18 c: cwcccccccccczcccjc\n1-4 n: pnnmn\n7-8 x: xzxxxxks\n17-18 p: fpwvhpqppcppppppplp\n2-4 d: bddsjwctx\n12-15 n: nnznnnnnnjnwknnn\n7-12 l: wlllllllllrrlzzllgj\n2-8 j: cdjfdjhjjsfj\n7-10 k: kkkwkkjkkc\n9-10 b: bbmbbbdtttbbb\n7-10 k: bqhccksknk\n3-4 f: zktp\n3-9 r: twjjkprkt\n3-4 p: pntp\n7-10 z: zzzzzzzzzzzz\n5-9 q: qtmqxtqkqqxbl\n14-15 d: dddddddzddddddddd\n6-8 b: bbbbbcbbb\n1-5 j: jjjzmt\n5-13 z: hqsfqszcwngzzvs\n15-17 k: kkkkkkkkkkkkkpskk\n3-7 f: fffnkfd\n8-11 g: gjgggjxggggg\n5-6 w: fqkswjjwkjnhwcz\n3-4 z: pgzzzhthd\n7-19 p: pppppplpbppppspszpp\n2-3 w: rwqxwv\n1-3 f: fgjfpnbvs\n4-9 s: wrxwxvfxlxvtsjswbns\n8-13 b: jbzldnnqbmghncwxbxvc\n1-4 m: rmmm\n6-7 x: txxbsjx\n16-17 c: ccccccccccccrcccxc\n6-8 d: ddqdddrxdjd\n9-13 j: zvzxjdwnjpqmz\n7-9 q: qqqqqqvqqqq\n8-13 n: nnnnnnncnnnld\n2-9 m: mmwqmzsvbmmmttm\n16-18 l: lpqzlhlljzplmllffs\n2-6 v: mcbjdxk\n4-6 b: vbpbnbqbjncxv\n7-17 c: rctcpcccbphccccvc\n15-16 c: chccccjcckccwckcccc\n3-5 p: rppgpcvxpdvdp\n2-5 t: lgktttd\n12-13 b: bbbdbbrblmbbsbbbbbt\n9-11 s: rqsdbzdsshgsj\n3-7 g: grgfjfkkslpbg\n7-10 n: qnnlgnknnnnnnn\n17-18 c: ccccccccdrccccccwcc\n7-8 g: gggggggstgg\n2-4 q: dvxq\n18-19 s: sssssssssssssssssss\n5-14 t: ttlvtttttftzfvt\n6-8 m: qsqmvmmmphnnmm\n10-12 x: xfxxxxxxxgxx\n9-14 m: mmmmmwmmmmmmmpmm\n11-13 l: lllxllllllllll\n4-5 m: mmmmf\n14-16 k: kkkkkkkkkzkkkkkt\n8-11 p: ppppppptpphp\n2-5 v: kbgmr\n3-4 h: hhcnh\n2-9 s: xljsstszh\n2-6 v: vvxvdhgx\n4-5 m: zmmmsmmmmp\n16-18 b: bbbbkbbbbbbbbbbbhf\n1-5 p: qppplp\n3-6 b: bjbkrgvb\n1-3 d: zddbwr\n4-20 d: dddddbdmqhdsdddddmdd\n4-7 r: rrrmrrj\n6-8 t: tttttdtttt\n2-4 w: fxwhh\n10-12 l: llhlllllllll\n11-13 j: jjjtgjbjjjfjcsjjs\n3-4 m: mmfmmc\n7-9 q: qgtzvqcqq\n6-14 x: dvkqqfnvvqxvwb\n2-3 d: wnzdj\n1-3 l: lvxfn\n4-5 r: rprzr\n10-16 k: zkklmdvkkkkmzvfv\n16-18 h: hhhhhhhthhhhthhhhvhh\n5-6 b: bnjbbr\n8-11 t: ttctjttvtttt\n2-6 r: rrrmrr\n1-10 w: cwfbkwwdkwgwgnsnhz\n13-18 b: bdkmbbbbmbvbpbbbblbb\n5-6 k: fkkkkb\n1-9 j: jjjjjjjjtj\n1-2 h: hhhpq\n14-16 v: vvvlvvgvvvvvvmfkn\n5-6 x: xxxxrnx\n12-14 h: hhhhhbhlhhhhhhhkhqh\n15-18 s: ssgrgdbtztjtcjsphs\n18-19 h: hhhhhhhhhqhhhhhhqbxh\n3-4 b: hbld\n16-17 x: xxxxxztfxxvsxxhhc\n14-19 s: ssssssmsssssswsmsxs\n6-7 p: pppppsppt\n2-3 c: ccclncs\n1-5 v: tvvvvvv\n5-8 x: kcxxxxqxxsxxgx\n1-3 w: wbqw\n11-14 g: ggggggggggsgggg\n1-5 m: kqsmlqtb\n7-8 j: zjjjhxjj\n10-11 v: vvvvvvvpvvbv\n6-14 q: tgqqtqjdqwqvqz\n1-2 g: gngg\n7-8 g: gcgggqmgglg\n4-9 h: hhhhhhhhthhhhhhhh\n7-13 r: hfzvpnrmlrbrl\n9-12 l: llllmflrxlll\n2-7 m: mwgpnpthsrzqdggkdw\n2-8 s: sssssfsss\n7-12 c: ccxncchccccc\n8-14 c: czcccczpcxxgcc\n3-6 v: dqtvhvpv\n12-14 v: vvvvvvvvnvvvvvn\n4-7 w: wwwwwws\n11-16 c: cccccpcctznshccc\n1-4 c: rfpcf\n5-10 p: pptpvplmpdrps\n16-17 q: rdwmkhrjbpftdszqqbc\n3-4 t: tgvt\n4-7 r: rrnrhnrrwrsrhhrrrtp\n13-15 t: tttvttttttttttstt\n11-12 x: xxgxxxxpxxxxxz\n1-10 g: xzbmfkzhdgfqqmwgxgrg\n18-19 x: xxxxxxxxxxxxxxxxxxx\n7-9 s: svsssssksnrsx\n12-14 p: pppmppmpppppphdppqvz\n4-5 w: wwwwxwwwww\n6-8 k: kkkvhjkpk\n4-5 j: jjjjj\n11-17 n: dnnnnnwrnnrnhnhnn\n1-5 h: mhhhh\n6-7 m: tqjhgmstv\n11-18 w: wjpczxkzmrwjqhhzfp\n12-13 m: mmgmmmmmmmmdmdlrm\n1-5 l: vlhblllltlr\n8-12 h: hphhchphjhhhhh\n3-4 m: fmmssm\n11-12 d: ddddddddddrd\n6-7 m: mmmxmmj\n1-7 h: twhhqhhnhlzx\n3-5 w: wbwwx\n4-7 b: bbxbbtnhhcbbbb\n8-10 m: mmdrwjmmpmmv\n15-19 s: sssxsssfsssssdbsbsss\n9-17 r: rrrrsrrrqrrrrkrrpr\n7-9 t: rdkctlttt\n2-5 j: jjjjjjj\n1-10 p: ppbgmjpwtpslzqp\n9-17 n: hsbbnwgvbmvrrssnn\n9-16 f: fbfffnfckffffffk\n4-6 m: mmmmmk\n5-9 n: njnnnnnnmfn\n11-13 z: zdzzzzfzzzzzn\n5-9 d: gwdbddddddf\n5-7 x: xxxxhxx\n3-10 s: hhssssspsgsswcc\n6-10 d: dddddfddddd\n15-18 l: gztzhfflpnkxhglmll\n6-16 l: lllllllshllllglwll\n4-9 t: tttkttttrtvtttqtz\n2-4 n: zcnnclg\n4-5 j: jzjjjj\n9-12 p: prppppppnpnpq\n13-15 p: mzrmqhnfpbxnwpppgtp\n2-5 f: gfffr\n1-3 n: nbnwnx\n19-20 j: jjjjjjjjjjjjjjjjjjmj\n3-9 p: pgpkphphpgp\n5-6 w: twpwwr\n3-6 w: brvnxhpwwwbn\n18-19 b: bbbsbbbbbbbbmbbbbrbb\n3-11 v: rxvcvbrggftgrzbhq\n9-10 q: qqqtqqqplw\n6-7 k: dtkgkfk\n12-19 n: bnnbpnlvnhwnnnnnxsf\n6-7 p: nppppjpp\n4-7 s: ssssssswszswsssssss\n17-18 n: nnnnnmnnnnnnnnnvznxn\n2-4 q: qqwqvv\n6-9 z: zdfzzqzdw\n4-6 s: svssfwfslmnxsd\n3-5 b: gmzbbp\n1-4 l: tbll\n1-4 h: htzhpdgzlhjbqwkjk\n1-4 t: mtttvrpt\n5-6 p: cmxkgpmspqq\n8-10 w: rwcpbwwvtvzhwwpwwwr\n11-12 w: wlwnhqwqwdwb\n1-4 p: ppbjcprpp\n10-12 s: spskpmsxhqssgh\n4-5 n: nnzns\n11-12 g: gwfgggggpgdghgg\n2-3 d: hdxzdsd\n3-4 g: gghj\n2-4 m: mmlcmmd\n11-12 t: ttttmttttttmt\n9-12 w: wmghwwwdtwww\n7-8 m: lzhpnrmz\n1-2 n: nnbn\n1-3 c: cltmcdlcc\n5-7 c: rmcmdxgxjq\n5-6 v: vvfvqt\n10-12 n: nnnnnnnnnnnnnnnn\n6-10 v: vvnfsvvvnqvjjvgptq\n10-17 b: bpbfbbbbbbqbbbbrbbbb\n4-12 v: vvvvvvvvvvvlvvvv\n8-14 n: bjfgpkpmnsvnnb\n7-8 w: wdwwwwwkw\n11-14 z: zzcwzzzzzvhzzzz\n12-15 z: mzzzzlzzznzlzzgzzzzm\n5-6 v: qnvmnv\n2-7 w: wbwsnww\n5-11 q: kqqpcqqpjmnlq\n13-15 l: plljllllllllwldlc\n4-5 l: kfcnbkhrnvtnrwn\n11-16 b: wmbfpdbdbbrbbbbdbbb\n2-8 t: jwswmtttg\n2-14 l: khqxxhmlndmdrtdxkbf\n1-6 f: ffdfprffvfkc\n5-10 c: cjcmqtdvhc\n1-8 v: vvvvvvvzvv\n2-3 b: xvjbs\n9-11 j: jvrjjcjjjjjvj\n7-12 w: wjwwwwwnwfwwww\n3-4 t: fppttghptsfkfdht\n4-5 l: nkllm\n7-9 j: wwlhsjnsj\n10-12 v: vvvlqkvbsdvvvwvjvvtv\n16-17 r: rrprrrrtrrrqrrrrrrr\n6-7 x: xqxxxdx\n6-14 x: mxlxxxxxtxxkbwxxxxx\n4-5 l: llsvl\n5-12 f: fbbvffxzfnfkwtn\n4-7 n: nnvtnhnnncbtnndr\n2-9 h: mhzfgjhcknskxqdk\n14-18 l: lqllllclllllqpllllll\n3-4 s: ssspss\n5-12 z: zzzzzzzzzzzwmz\n18-19 p: ppcpppwppppppppppvgp\n3-4 z: qzzzzz\n5-7 x: xrxxxxx\n3-5 z: zzzjr\n3-4 j: jjfjj\n5-6 p: pfpppnp\n3-4 j: jjnm\n8-16 m: mmmmmcmmmmmmmmmm\n2-6 d: tddddbdqdd\n3-8 d: qkflfddfdrvsj\n8-14 m: cwfmsqvmllbdnjvgp\n3-7 h: xhjtdhhc\n1-7 v: vjvmvvd\n6-10 j: jjjjzjjjjfjjjjjr\n8-9 x: xbxxxxgxxqx\n3-5 f: fffpf\n2-8 n: nnlnbndn\n4-13 v: vvvqzvvvfvvvvv\n4-7 s: tsrcslsrdx\n3-6 h: lhhhpr\n1-5 r: rtvxw\n3-5 m: mqmkvmmm\n3-4 p: ppmp\n2-3 j: jjfj\n2-13 v: vgzvvvvvvwvxdg\n14-15 q: qxqqqqqqqqqvqwqqq\n9-12 h: cjzzthfchxth\n3-7 p: pbspspkp\n8-9 n: vnnngnxlnnnn\n10-16 g: dngspgdnnggggsgqrg\n5-6 n: nnnnnqn\n3-4 l: cwtl\n1-6 r: hrrrrrr\n2-5 b: blwbz\n11-18 d: ddddddtdddxdddddddd\n3-6 k: lkxkkqks\n15-17 j: jjjjjjjjjjjjjbqsjf\n13-15 k: sxlplkjfqwmpkjk\n9-10 l: llllllllmlbl\n5-10 g: ggsgxggggg\n1-2 f: twwf\n2-4 d: kwdd\n4-13 g: cwczjzgqtlhsg\n12-14 z: kckzccljvrvzgz\n9-12 t: wtttxttttxttdt\n14-15 m: zmmmmmmmmmmmmbw\n3-4 r: frrtwr\n4-7 k: ckkmkkdkkkkk\n16-19 k: clzjfkkdljgnskrfjkk\n6-8 w: fwphnwzb\n14-15 p: pppppjpppppppbbp\n6-8 q: qqqqqqqz\n10-11 j: gdrjbjzzfjqwvntppjtb\n14-15 m: hmmmmxmmbmmmmmmmmm\n8-12 b: bbbbtqggbsbwbsbb\n1-2 g: gcgzl\n1-2 j: jmwv\n3-5 q: qqqqwqq\n3-6 b: bbcbbbbb\n5-6 l: lqlllfl\n6-9 f: ffffffffsffmffff\n11-20 n: nnnnnnnnnnwnnnnnnnnn\n3-4 w: qvfn\n2-8 x: zzxxjxxxx\n3-4 r: pbcrr\n10-12 t: ttvgcttctjwt\n8-12 g: gnggggvzgggg\n7-9 z: zzzzzzzszz\n4-5 c: ncclccc\n5-6 t: ttttttt\n4-6 q: qtqzjq\n10-20 p: hqxflpbmpppspqhptmpp\n2-5 k: bblpkkkkkjlgvzsrhtvk\n2-7 l: clllpvlzvdnllflll\n9-20 p: pppppppqhppppppppppp\n12-13 v: vvmvjkvhvvvvdpnt\n13-15 x: xxxxqxsxzxxncxxxxx\n11-13 n: nnnnmtnsbnnnp\n10-13 m: mqmmmmwfmvmmmmmm\n5-8 b: bbbbkbbb\n11-12 h: hhhqhhhhhhmgh\n4-6 v: vvvvvqvvvv\n2-5 j: rsbnjnxrmhm\n2-8 j: jjjjjjjhj\n5-7 q: rqzkpgj\n6-12 b: bbbbbbbbbbbzbbb\n2-4 h: hhqv\n4-6 c: gkxfcwsctcz\n1-6 t: sfqlpthhp\n7-8 g: gfpgggrzg\n1-5 v: vvvvvvv\n5-6 q: qqsqllqq\n15-16 b: bblbbbbrbtbbmbbbz\n5-12 t: btsttjttttbgrwrrlbg\n7-12 v: qvvvjvvcvvvpvvvv\n19-20 l: nvlxxlxvllllcsdnwldl\n9-13 w: wwwmwwwhwwwwvww\n2-4 x: lxldmxxhkzxrpzbh\n11-12 n: nnnnnnnnnnjn\n3-8 b: xbflzhbbbbbbbbbbbbbb\n15-18 s: spssdnxffsksmxssbt\n4-5 c: cnjccxcc\n1-2 w: cwww\n7-8 d: cddlddwddgdl\n9-12 p: pccppppchrpphppppp\n3-7 x: drxcxcx\n9-14 k: jdkmxbkrxnnnrkkw\n6-18 z: frpczwtszjbgfdszzj\n1-4 n: qfnjfn\n12-14 g: zggggphlgvghggggg\n5-15 l: wfkwtnsllxlzqzq\n2-12 t: tfvtrmtttttpttmttttt\n2-4 c: mvjct\n4-10 x: mcxlhxvpkdbz\n7-18 w: wwxzwqxbwwswwjfmsw\n3-4 s: jssnss\n9-11 j: jzjjjjjjjjq\n1-16 s: dssssssssssssssss\n3-10 d: cndlgskktdl\n4-9 c: rcwlcgccccz\n11-13 w: wpwwwwwwwwwzwwww\n5-6 v: vvvvkh\n8-13 z: zzzkzzzzzzklzzzdzz\n2-11 v: dpqghsvhvsvfq\n9-13 f: vrjfflzfjfsfszrxftff\n11-14 j: jjcqcfhxspjjjgsjvgj\n2-6 l: lcldhcfv\n7-10 l: hjrcprhcll\n4-6 q: sqqqvq\n1-2 g: fggzc\n1-9 k: skkgknkdnklnkcsmp\n3-4 d: dbdv\n8-11 m: mnmmjmmmmmm\n2-4 d: ddgbhdrdn\n1-3 v: hvvv\n6-7 f: fffffzt\n4-6 t: ltttvs\n2-3 n: npnrr\n1-19 b: pbbbbxbbbgbbbjbbbsbv\n9-20 w: mwwwwjwwzwqwbwwwqwkw\n11-12 z: zzzzzzzzzztjz\n1-6 z: zzzzzdzznzm\n4-5 n: nnsnn\n7-10 v: qwkvvzvwvmvmvkvvvwvl\n2-5 x: vqgzdvbqrdxpfnzzjxv\n11-14 j: jjjljjjjjjjjjz\n1-3 n: jndnn\n2-4 l: lbxxlllcv\n5-6 s: sssskg\n11-14 l: llllllllllsllll\n2-5 s: swdszr\n2-4 q: qpqc\n9-11 h: ghsbhhgzhgxzhfhhhhhh\n11-12 v: vvvvvvvvvvvn\n2-3 r: rrhr\n10-11 w: gmwtmnppnqz\n9-13 f: fffffpfrffffff\n1-6 c: cvtwcptzkq\n11-13 k: zkkkklskkkkkskkk\n3-8 p: ctpkfpspplpbz\n9-10 b: bbbbbbbbbbb\n6-14 d: dddddddddddddddd\n8-9 d: mdddrddzdd\n6-7 v: vvvvvvxvvvvv\n4-6 g: gggtntghnvgrfj\n4-8 p: zbpmzjfplphp\n5-7 h: hhhfhwm\n6-19 g: jbkxvgqzdngcclvlhxgg\n8-10 q: qhpmqqwfqnbthqwq\n1-3 h: hhrpg\n7-10 j: jmjkjjgcrj\n2-3 g: gkgggg\n3-4 l: lsql\n7-13 c: cckccccccccccc\n5-9 v: zcfvxvnvbvvccqvkvv\n4-5 h: hqchnh\n4-5 v: vfzvrfvkv\n2-3 f: ftlf\n17-18 r: rqrrqrrnvrsrrrdrrt\n5-8 w: pwwnwhwwvwwww\n3-13 b: hwhhtvcjbgcpbrqpg\n6-14 z: zzzzzgzzzzzzlz\n3-4 s: zssr\n4-7 s: lfstsssjs\n7-9 q: xqqqqqnqqq\n11-14 g: gggggggggggggg\n5-15 m: mzjpmvmpnfkpmtrj\n1-2 z: bzzzzzzzzzz\n3-5 b: xbbbmp\n5-8 m: mmmxxxmm\n13-14 b: bbbgbbbbbbbzbb\n5-6 s: sstsxfssgvn\n2-10 w: dpwdwswdwwwwn\n4-6 v: xvnvvv\n1-3 d: ddqd\n5-8 f: ftzffftgfgkhjff\n10-15 g: gggggggkggggggg\n10-16 h: hfhhhzhbthhhxhhhjh\n2-10 n: ntnnnnnnnnn\n8-9 d: dqxcnxsdwmbd\n1-3 z: zlzqzzsnlrt\n3-8 b: cgtlsbbbbn\n3-5 k: kwkktt\n5-10 s: cscdhgsxns\n1-13 w: pwwwwwwwwwwcgswwwwww\n1-4 d: dddd\n17-20 v: vvvvvvvvvvvvvvvvvvvn\n6-10 k: kkkkdkrjkqpkvckc\n7-8 g: fgglcggg\n5-10 q: qqqqssqqqqq\n12-13 j: jhjjjjjjmjjjsjj\n3-4 c: czlx\n2-7 g: ggmjwww\n3-11 f: mdfbtbjvftnsvnnlmzg\n2-6 b: clqwlbslldxmh\n3-6 b: bcbbxbk\n5-6 d: grdrfrdddvcq\n14-18 c: ccctccjccccccwvccc\n5-6 h: hrhhhp\n1-7 b: bbbbbbbb\n1-8 w: mwwnwwww\n4-5 j: jjjlcjj\n6-16 m: mmcmmmxmmzmmrmmfmmm\n7-12 p: ppppptpppprp\n3-8 s: sfrssssspfkskdbdsq\n14-19 x: xxhrbxtjxwxcxgxxvpxx\n3-10 t: tttvtqkltrtgttttzstg\n2-6 r: xrgrjrlfjkf\n9-10 g: ggggggggjq\n2-5 b: pblbzcbrsjnbl\n9-11 z: zhzcnzfclxrbjzzzzzz\n7-8 z: zzzzzzzk\n4-5 q: qqqqg\n1-6 b: drfdfmkjhb\n12-16 x: mjkxsmrcvjlkxbtxld\n7-11 m: nmfflxpxkqmpdswsm\n2-8 n: rnnbqkfqkl\n11-15 l: rgpmvlrkgqnglkgbqpq\n4-7 g: gggfgjgg\n5-10 h: hhhhhmgwhhdphc\n16-18 s: dzzkhzpsssqcspdsgs\n2-8 c: cccccccq\n8-9 s: hssstqksksss\n2-13 l: zlxbldnvnnznswl\n6-7 x: xxxxxxxzx\n2-7 h: thzhwhxthrh\n3-4 j: jmjt\n1-2 s: sswf\n8-15 n: nnnnnnnnnnnnnnw\n2-5 s: xssws\n6-7 p: pppppqtp\n2-6 b: twvbdbqbd\n1-2 l: lwdpflk\n11-14 k: kkkrkkkkkdjkjpk\n4-8 x: vhfxdxxsxxxv\n13-14 p: pplnppppppprmppp\n16-17 s: xqqkssgpsjpqsstks\n1-4 k: kdkkk\n11-12 c: glqzcxcsctcl\n7-12 h: hhhhhshhhhhhhh\n6-13 l: mlqsllllbrlcrllbcllv\n8-13 z: zgwzzczzzzzlzpx\n6-7 v: qqqvxdgprhxv\n3-5 x: xxbxxx\n8-11 h: hhprrhnttdhhxshgqhhl\n2-3 p: rpppgpp\n6-8 d: dkddsgddddd\n2-3 k: xkkknk\n8-12 w: swrwzzcwdrgjh\n9-10 k: kmkkkpkklkkkkkk\n13-15 c: cccccccccccchcwc\n3-4 g: gjgn\n7-8 q: qqqqqqqvq\n11-17 t: hbtmtpmzcfqqlcshppf\n3-5 l: lllbk\n4-6 f: fffffjfffmff\n15-16 x: xxxxxxxxxxxxxxwxx\n4-5 q: fqmnjq\n7-13 w: wwswrpkwqnwwmwbzkkw\n1-2 g: glxdfx\n8-12 l: lslllllmlgglll\n2-3 l: rgghlvwlvbthql\n13-16 m: mmmrmmmmmmmmlmwnmmsm\n4-5 n: nnnnn\n4-20 p: dvfpgxspwwphpgfczzzz\n5-7 r: rrrrrrc\n5-12 h: hlhshhhhdhhhh\n1-4 s: pssb\n10-14 q: qqgblzqmnnqqqq\n3-4 d: lclh\n3-10 k: kkvkklkkklckkdkl\n4-5 k: cdjzvk\n4-5 m: mmmrm\n3-7 l: hhzwdglntjlhdmfkqjf\n8-18 n: nnnnnnntqnnhnndnnl\n2-8 k: kxkbqkgkkmlkk\n1-4 w: wwpwwfmhwhs\n1-8 r: kbplbrnrdd\n4-11 w: bhlwpttwbkwnkhcdkc\n11-15 j: bqbmrjtwrtjqsppm\n1-8 v: vjvtvwwbgc\n4-5 m: wcjmj\n4-11 f: jtlfffrnffhfp\n9-11 j: jjjjjjjjzjjjx\n3-8 v: svvvvqvlzvvnvz\n5-7 v: vvvvrvvtvpv\n8-9 s: srzhssrbssssvvsxlm\n7-8 p: mjphpdtpn\n4-5 d: qzxddrthrdljdrqslzdd\n7-10 d: ddddddpdvsd\n2-6 x: txdxbxxmxrw\n6-16 m: zdbmhmmmmkmmrmmjw\n3-4 v: vtvp\n4-13 b: bbttmmrrzcgxbc\n1-3 j: fdjb\n11-13 k: kkfkkkkkkkkzkk\n6-9 k: pbgxlkwwkwsp\n5-12 t: ttqztdtttttt\n5-10 q: qqqqqqbggq\n4-5 p: ppllr\n1-3 b: tsvxbgbkmm\n4-8 b: bhbzbbgb\n1-4 w: jlwsww\n15-18 l: llllllllllmlllllnw\n6-13 d: ddwddjddddddhmdd\n3-5 j: jcjdf\n7-9 j: njrzjjjjx\n7-10 j: jjjjjjjfjjjpjx\n3-4 c: zcxcv\n4-8 z: skqzvzrnzzclzzzzxrdn\n8-13 n: nnnnnnnqgnnrn\n5-6 x: rmzcxxxxcxkdxqmlxb\n9-14 n: nnnnnncnjnnqnxwnn\n11-14 b: bbwbbbbbbbbbbhbwbbb\n17-18 j: pjwjjjjjjjjjjjjjjzj\n3-6 q: qqfqqz\n3-16 t: wrksrrghptphntjtl\n8-12 n: nnnnnnnqnnnnnnn\n5-6 p: pphcpxsbp\n4-8 b: bbbblbbnhbq\n1-5 w: wwslcwdw\n1-9 t: zkttjttptp\n5-8 n: snnfnnndnnn\n14-16 s: sssssssssssssrsssss\n2-3 c: wctjrbccl\n12-14 t: tkjttfpptxbtdvttbwtn\n12-13 h: hhhhhhhhhhhjh\n7-12 t: wttqzsfttftqkt\n5-9 t: xdtvtqvztvttrkztkhtt\n2-3 n: mnznz\n18-20 t: npbfsgxtfbklzcrrqftt\n14-15 g: gjggggggggggggg\n1-4 v: vvvvvv\n8-9 h: hhhhhghht\n1-12 z: zzwwzzzzzczmzcdl\n10-15 m: mmmzmbmmmmmmmwfmmcjm\n10-11 g: gggggmjgtggggg\n2-3 z: zczzrzxvvbfvfggzmq\n7-17 g: gkxmjwgnggrxbrtlgqgg\n17-18 z: zzzhzzzzzzzzzzzzzf\n2-3 r: rcrpstlplrsrx\n4-8 j: fpjkzjjs\n2-6 l: chlglllqljllldll\n12-14 d: mwkdrghdvhdddl\n7-11 q: dqqtvzxsrsnjqkfrf\n12-14 p: tpppppppppppspp\n5-6 m: jmmmmlm\n1-7 c: dccdcccqcrc\n1-4 t: tttn\n2-8 c: xcbgcqlw\n13-14 s: brlmjgbjswwqsvhxsss\n16-19 g: gggfggggggggggggggtg\n5-13 n: nnfwnnqnntnnnnbnnp\n11-14 d: dddddrdmdsfddddd\n5-13 x: xxxxxxxfmxxxcxxxdv\n4-5 x: nvxxq\n10-15 q: qqqqqmqqqqwqbqqbq\n13-14 g: gggggggggggggg\n5-6 w: wtwwnwmw\n2-4 f: fdff\n1-2 b: gbxvwwzbkccmlmfjglx\n4-6 s: sfwgss\n3-11 w: wwtrskwrwqwswhwwwn\n5-14 j: ffzjjvcfjszmxjxdmbv\n11-18 r: rrrrrrrrrrdrrrrrrsr\n6-7 j: jjjjjjvj\n5-13 h: hhpkvvmnrbxjhwhzppft\n16-17 t: ttftttvttttxttttt\n3-6 p: kvqmppp\n10-14 v: pvzvmvvtvvvmvclmv\n4-14 w: xdhxtwzwdsmgwwwgb\n6-11 p: ppmpdpgtpbxvspgppg\n11-19 f: fwfwpfkfqffffrfxfff\n17-20 x: xxxxxxxxxxxxxxxxgxxg\n1-2 h: hpfcktmbsjgdtgrx\n3-4 c: dcvz\n6-7 b: pwmqpbgnbfb\n13-18 h: cxpmjhhghnthhqfxhp\n1-5 z: zzzzmlzzzzzzzzzzzz\n5-15 k: kkkkpkkkkkkkkkkkkkkk\n6-8 l: lwlwslkkvzkdbpzzqp\n5-10 x: xxxvxxjxxd\n4-5 k: kbkkqk\n3-10 v: vvgvvvvrvzvvvvvvlvvv\n4-5 g: ggggjgggg\n14-17 v: vvvvvvvvvvvvvvvvk\n5-8 p: twpphnmvqplcxpp\n11-16 s: ssssswshsshcsssrdks\n10-11 z: zzjzzzzznqzzz\n1-5 r: rrrrr\n8-9 d: dddddddpdd\n3-6 h: hhhhhh\n4-5 t: ttbrt\n15-16 j: gbjqljxjpjcsjjjrjg\n7-8 t: rncvbfct\n11-13 t: lltjtdttttthvtttttv\n14-15 p: ppppppppbppppppw\n1-4 r: rrrrmzc\n6-8 k: kfqkfkkrdkkm\n5-7 r: rbzlrwwprrr\n3-17 s: ssjsssdssvwsssssss\n6-7 g: rgdgggk\n4-8 k: kkkkkkkjkkk\n9-10 q: qqqqqqqqtq\n7-12 x: bztpxxxdmzvrpxxk\n1-2 z: zpzccz\n6-16 n: nncklnttbwhfvdnbf\n9-11 b: bmbbcbbbdbt\n8-9 c: cccpcccchccbb\n12-13 b: bbbbbbbbbbbbs\n9-18 j: qrssnrgmjsvfmjzwsj\n8-9 r: mrmbrrnrdprzrrfrb\n5-8 h: hhhhzhpghq\n6-7 n: nqnnptnbn\n18-19 q: gkqqdqlqmdzkbbrnfhs\n15-16 f: sfbcsbgxzhrzjndc\n12-14 t: ttttntttttthtt\n6-7 d: mpdddddn\n6-15 c: lzqjqtmtzqpmjccx\n7-17 m: mmmmmmmmmmmmmmmmgm\n1-10 s: sssslhssssss\n5-8 j: slmkjlddltj\n2-16 l: ldlllclzffklzzvllll\n10-11 r: rrrrrsdrrbr\n13-14 k: kkkkkkkkkkkkkk\n5-6 c: brncpc\n11-13 v: vvcvvvvvvzvvnv\n12-14 z: zzzzzzgzzztdcwxptzf\n1-5 l: bllldlx\n3-5 g: rgcxgdsxdqgzgkg\n7-8 n: vrspnqnbn\n2-12 l: jlsllljmllklllll\n2-6 n: ncnvnn\n7-14 z: rpxzqzczzlzsnzzzqbn\n8-14 f: fmffhhljlffsffffsf\n4-10 c: ccdcvpcbjgtwjrcl\n9-10 w: wwwwwwwwwm\n3-4 s: wssw\n12-17 c: cccccccsxcqbccccccp\n12-19 z: zdzgzlzbzzzzzzzzzzq\n5-6 f: svfbqvfffsf\n4-9 z: zzzxzhzzz\n2-15 p: pppnpppnppppgpwpppw\n6-9 t: ntftnzjntd\n14-17 m: mmmmmmmmmmmmmmmmkm\n8-9 z: zzzzhptzzzdfzgwzsz\n16-17 g: ggcgqzgggggggggtzgg\n10-13 x: xxxxxxdxxxxxx\n14-16 d: dfdddddddddddddld\n3-4 d: ddwd\n7-15 w: zcwtmgtwwwbhfwvww\n18-19 c: ccccccccccccccccccs\n2-8 f: qqrfxvrtvttq\n4-5 l: ltjll\n5-6 h: hvshchzhh\n3-9 f: ffvvfffffff\n5-6 g: ggggxw\n7-10 p: zmgppmpqfgp\n3-6 b: pgvdvbb\n18-19 k: kkkkkbkkkkkkkkkkkjg\n5-7 w: hwgwwzp\n4-5 h: nhdgfthhhvh\n1-4 z: hvvzrkfz\n15-16 s: ssvgjfsnsplspsvjw\n4-8 p: pjjzwpnpfkpzpppv\n2-3 w: whwmsq\n4-9 d: mjvfdndmzddd\n2-4 f: pgff\n1-4 f: ffnk\n18-19 s: ssswcssgssssssfslssl\n5-7 n: rplqnvhd\n7-13 s: bsbsskzsfsrss\n9-19 j: jjjjjjjjdjjjjjjjjjl\n5-7 m: kfwmzqm\n5-9 h: hhlhzhhhh\n4-6 z: twvnszwzx\n7-10 g: gggjggnggdggngggs\n12-14 f: ffffffffffffflf\n3-5 g: gzgsp\n2-6 d: dndddd\n5-10 r: rkzffghrrltgrrpmrr\n1-3 b: bcfbbs\n10-12 b: bvbqdbbbbbbkb\n3-10 v: hpvvvvhxsfjvdj\n2-3 t: ttttt\n1-2 r: fmrf\n3-4 z: zqzkxj\n14-16 d: ddddddbddgddsdjddd\n12-14 l: lllllllwllllgl\n3-4 j: mvsjjrzxtsbbk\n2-6 k: pkkkkjdk\n12-17 k: krkkkkzkkkkhkkghk\n2-4 v: bnfvknzw\n2-3 q: qqqq\n3-7 q: kjgqqpqqrcrq\n3-9 j: wfjjzjjjg\n9-13 c: fccccvccccccj\n3-4 f: jrffdsfdnr\n9-13 x: xxxxxxxxxxxxxxxx\n2-4 c: cbccc\n4-5 q: cwmhd\n6-17 z: zvzplhczbrzwpkcqzdzm\n9-14 f: vdvdnbvxxmjsftlfbqs\n1-5 t: thmwhctkpsj\n2-3 t: xtvtfnxhdht\n2-5 x: nvxrx\n9-14 v: wpvtmgfvqdmnvvvq\n6-7 l: fclxwlfblfrvlblc\n2-6 g: cttgfsk\n8-14 r: vrrrrgrmrrrrhrrrbr\n3-7 r: rrrrrplr\n5-18 j: snghjbpjctwgjcftjt\n17-18 x: xxxxxxxxxsxxxzxxxjxx\n9-10 q: rzqqqqqqqzq\n13-19 w: lqwpwwwcwwwwgpwtpww\n10-11 x: tvcxzxwpxxx\n13-14 k: kkkkkkkkkkjkfkk\n11-16 w: wwwwwwwwwwqwwwnl\n4-5 v: vslnvv\n3-5 h: hhshj\n5-6 m: npkmmw\n2-11 q: nbcbjbcwknqxrkqqwhq\n9-10 p: pbppjpppnpppp\n10-15 d: drdsdddlddddnddqdwd\n1-2 d: pdhd\n12-17 w: wwwwwwwwwwhmwgwwwwww\n4-5 z: zmtztrtwfntphzzrlj\n7-13 p: pvrbkxppsppxpppmqz\n1-6 s: ngsssssrbs\n7-8 j: jjjjjjzhj\n8-16 c: cpcpcqccrnckpmcw\n7-8 x: xxnszxbxgxwxx"

  val splitData = rawData.split("\n").toList

  val formatData: List[List[String]] = for (set <- splitData) yield {
    set.split(" ").toList
  }

  val finalData = for (set <- formatData) yield {
    for (num <- set.indices) yield {
      set(num).split("-").toList
    }
  }

  def validPasswords(data: List[IndexedSeq[List[String]]]): Int = {
    var validPasswordsList = new ListBuffer[String]()
    for (set <- data) yield {
      val lowNum: Int = set(0).head.toInt
      val highNum: Int = set(0).last.toInt
      val key: Char = set(1).head.charAt(0)
      val password: String = set(2).head
      val count: Int = password.count(_ == key)
      if (count >= lowNum && count <= highNum) {
        validPasswordsList += set.toString
      }
    }
    val numberValid: Int = validPasswordsList.length
    numberValid
  }

  def secondPartValidPasswords(data: List[IndexedSeq[List[String]]]): Int = {
    var validPasswordsList = new ListBuffer[String]()
    for (set <- data) yield {
      val firstPosition: Int = set(0).head.toInt - 1
      val secondPosition: Int = set(0).last.toInt - 1
      val key: Char = set(1).head.charAt(0)
      val password: String = set(2).head
      if (password.charAt(firstPosition) == key && password.charAt(secondPosition) != key) validPasswordsList += set.toString
      else if (password.charAt(secondPosition) == key && password.charAt(firstPosition) != key) validPasswordsList += set.toString
    }
    val numberValid: Int = validPasswordsList.length
    numberValid
  }

  println(validPasswords(finalData))
  println(secondPartValidPasswords(finalData))
}