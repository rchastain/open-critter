
const
  PgRandomPiece  = 0;
  PgRandomCastle = 768;
  PgRandomEP     = 772;
  PgRandomTurn   = 780;

const PgRandom: array[0..780] of TKey = (
  TBB($9D39247E33776D41), TBB($2AF7398005AAA5C7), TBB($44DB015024623547), TBB($9C15F73E62A76AE2),
  TBB($75834465489C0C89), TBB($3290AC3A203001BF), TBB($0FBBAD1F61042279), TBB($E83A908FF2FB60CA),
  TBB($0D7E765D58755C10), TBB($1A083822CEAFE02D), TBB($9605D5F0E25EC3B0), TBB($D021FF5CD13A2ED5),
  TBB($40BDF15D4A672E32), TBB($011355146FD56395), TBB($5DB4832046F3D9E5), TBB($239F8B2D7FF719CC),
  TBB($05D1A1AE85B49AA1), TBB($679F848F6E8FC971), TBB($7449BBFF801FED0B), TBB($7D11CDB1C3B7ADF0),
  TBB($82C7709E781EB7CC), TBB($F3218F1C9510786C), TBB($331478F3AF51BBE6), TBB($4BB38DE5E7219443),
  TBB($AA649C6EBCFD50FC), TBB($8DBD98A352AFD40B), TBB($87D2074B81D79217), TBB($19F3C751D3E92AE1),
  TBB($B4AB30F062B19ABF), TBB($7B0500AC42047AC4), TBB($C9452CA81A09D85D), TBB($24AA6C514DA27500),
  TBB($4C9F34427501B447), TBB($14A68FD73C910841), TBB($A71B9B83461CBD93), TBB($03488B95B0F1850F),
  TBB($637B2B34FF93C040), TBB($09D1BC9A3DD90A94), TBB($3575668334A1DD3B), TBB($735E2B97A4C45A23),
  TBB($18727070F1BD400B), TBB($1FCBACD259BF02E7), TBB($D310A7C2CE9B6555), TBB($BF983FE0FE5D8244),
  TBB($9F74D14F7454A824), TBB($51EBDC4AB9BA3035), TBB($5C82C505DB9AB0FA), TBB($FCF7FE8A3430B241),
  TBB($3253A729B9BA3DDE), TBB($8C74C368081B3075), TBB($B9BC6C87167C33E7), TBB($7EF48F2B83024E20),
  TBB($11D505D4C351BD7F), TBB($6568FCA92C76A243), TBB($4DE0B0F40F32A7B8), TBB($96D693460CC37E5D),
  TBB($42E240CB63689F2F), TBB($6D2BDCDAE2919661), TBB($42880B0236E4D951), TBB($5F0F4A5898171BB6),
  TBB($39F890F579F92F88), TBB($93C5B5F47356388B), TBB($63DC359D8D231B78), TBB($EC16CA8AEA98AD76),
  TBB($5355F900C2A82DC7), TBB($07FB9F855A997142), TBB($5093417AA8A7ED5E), TBB($7BCBC38DA25A7F3C),
  TBB($19FC8A768CF4B6D4), TBB($637A7780DECFC0D9), TBB($8249A47AEE0E41F7), TBB($79AD695501E7D1E8),
  TBB($14ACBAF4777D5776), TBB($F145B6BECCDEA195), TBB($DABF2AC8201752FC), TBB($24C3C94DF9C8D3F6),
  TBB($BB6E2924F03912EA), TBB($0CE26C0B95C980D9), TBB($A49CD132BFBF7CC4), TBB($E99D662AF4243939),
  TBB($27E6AD7891165C3F), TBB($8535F040B9744FF1), TBB($54B3F4FA5F40D873), TBB($72B12C32127FED2B),
  TBB($EE954D3C7B411F47), TBB($9A85AC909A24EAA1), TBB($70AC4CD9F04F21F5), TBB($F9B89D3E99A075C2),
  TBB($87B3E2B2B5C907B1), TBB($A366E5B8C54F48B8), TBB($AE4A9346CC3F7CF2), TBB($1920C04D47267BBD),
  TBB($87BF02C6B49E2AE9), TBB($092237AC237F3859), TBB($FF07F64EF8ED14D0), TBB($8DE8DCA9F03CC54E),
  TBB($9C1633264DB49C89), TBB($B3F22C3D0B0B38ED), TBB($390E5FB44D01144B), TBB($5BFEA5B4712768E9),
  TBB($1E1032911FA78984), TBB($9A74ACB964E78CB3), TBB($4F80F7A035DAFB04), TBB($6304D09A0B3738C4),
  TBB($2171E64683023A08), TBB($5B9B63EB9CEFF80C), TBB($506AACF489889342), TBB($1881AFC9A3A701D6),
  TBB($6503080440750644), TBB($DFD395339CDBF4A7), TBB($EF927DBCF00C20F2), TBB($7B32F7D1E03680EC),
  TBB($B9FD7620E7316243), TBB($05A7E8A57DB91B77), TBB($B5889C6E15630A75), TBB($4A750A09CE9573F7),
  TBB($CF464CEC899A2F8A), TBB($F538639CE705B824), TBB($3C79A0FF5580EF7F), TBB($EDE6C87F8477609D),
  TBB($799E81F05BC93F31), TBB($86536B8CF3428A8C), TBB($97D7374C60087B73), TBB($A246637CFF328532),
  TBB($043FCAE60CC0EBA0), TBB($920E449535DD359E), TBB($70EB093B15B290CC), TBB($73A1921916591CBD),
  TBB($56436C9FE1A1AA8D), TBB($EFAC4B70633B8F81), TBB($BB215798D45DF7AF), TBB($45F20042F24F1768),
  TBB($930F80F4E8EB7462), TBB($FF6712FFCFD75EA1), TBB($AE623FD67468AA70), TBB($DD2C5BC84BC8D8FC),
  TBB($7EED120D54CF2DD9), TBB($22FE545401165F1C), TBB($C91800E98FB99929), TBB($808BD68E6AC10365),
  TBB($DEC468145B7605F6), TBB($1BEDE3A3AEF53302), TBB($43539603D6C55602), TBB($AA969B5C691CCB7A),
  TBB($A87832D392EFEE56), TBB($65942C7B3C7E11AE), TBB($DED2D633CAD004F6), TBB($21F08570F420E565),
  TBB($B415938D7DA94E3C), TBB($91B859E59ECB6350), TBB($10CFF333E0ED804A), TBB($28AED140BE0BB7DD),
  TBB($C5CC1D89724FA456), TBB($5648F680F11A2741), TBB($2D255069F0B7DAB3), TBB($9BC5A38EF729ABD4),
  TBB($EF2F054308F6A2BC), TBB($AF2042F5CC5C2858), TBB($480412BAB7F5BE2A), TBB($AEF3AF4A563DFE43),
  TBB($19AFE59AE451497F), TBB($52593803DFF1E840), TBB($F4F076E65F2CE6F0), TBB($11379625747D5AF3),
  TBB($BCE5D2248682C115), TBB($9DA4243DE836994F), TBB($066F70B33FE09017), TBB($4DC4DE189B671A1C),
  TBB($51039AB7712457C3), TBB($C07A3F80C31FB4B4), TBB($B46EE9C5E64A6E7C), TBB($B3819A42ABE61C87),
  TBB($21A007933A522A20), TBB($2DF16F761598AA4F), TBB($763C4A1371B368FD), TBB($F793C46702E086A0),
  TBB($D7288E012AEB8D31), TBB($DE336A2A4BC1C44B), TBB($0BF692B38D079F23), TBB($2C604A7A177326B3),
  TBB($4850E73E03EB6064), TBB($CFC447F1E53C8E1B), TBB($B05CA3F564268D99), TBB($9AE182C8BC9474E8),
  TBB($A4FC4BD4FC5558CA), TBB($E755178D58FC4E76), TBB($69B97DB1A4C03DFE), TBB($F9B5B7C4ACC67C96),
  TBB($FC6A82D64B8655FB), TBB($9C684CB6C4D24417), TBB($8EC97D2917456ED0), TBB($6703DF9D2924E97E),
  TBB($C547F57E42A7444E), TBB($78E37644E7CAD29E), TBB($FE9A44E9362F05FA), TBB($08BD35CC38336615),
  TBB($9315E5EB3A129ACE), TBB($94061B871E04DF75), TBB($DF1D9F9D784BA010), TBB($3BBA57B68871B59D),
  TBB($D2B7ADEEDED1F73F), TBB($F7A255D83BC373F8), TBB($D7F4F2448C0CEB81), TBB($D95BE88CD210FFA7),
  TBB($336F52F8FF4728E7), TBB($A74049DAC312AC71), TBB($A2F61BB6E437FDB5), TBB($4F2A5CB07F6A35B3),
  TBB($87D380BDA5BF7859), TBB($16B9F7E06C453A21), TBB($7BA2484C8A0FD54E), TBB($F3A678CAD9A2E38C),
  TBB($39B0BF7DDE437BA2), TBB($FCAF55C1BF8A4424), TBB($18FCF680573FA594), TBB($4C0563B89F495AC3),
  TBB($40E087931A00930D), TBB($8CFFA9412EB642C1), TBB($68CA39053261169F), TBB($7A1EE967D27579E2),
  TBB($9D1D60E5076F5B6F), TBB($3810E399B6F65BA2), TBB($32095B6D4AB5F9B1), TBB($35CAB62109DD038A),
  TBB($A90B24499FCFAFB1), TBB($77A225A07CC2C6BD), TBB($513E5E634C70E331), TBB($4361C0CA3F692F12),
  TBB($D941ACA44B20A45B), TBB($528F7C8602C5807B), TBB($52AB92BEB9613989), TBB($9D1DFA2EFC557F73),
  TBB($722FF175F572C348), TBB($1D1260A51107FE97), TBB($7A249A57EC0C9BA2), TBB($04208FE9E8F7F2D6),
  TBB($5A110C6058B920A0), TBB($0CD9A497658A5698), TBB($56FD23C8F9715A4C), TBB($284C847B9D887AAE),
  TBB($04FEABFBBDB619CB), TBB($742E1E651C60BA83), TBB($9A9632E65904AD3C), TBB($881B82A13B51B9E2),
  TBB($506E6744CD974924), TBB($B0183DB56FFC6A79), TBB($0ED9B915C66ED37E), TBB($5E11E86D5873D484),
  TBB($F678647E3519AC6E), TBB($1B85D488D0F20CC5), TBB($DAB9FE6525D89021), TBB($0D151D86ADB73615),
  TBB($A865A54EDCC0F019), TBB($93C42566AEF98FFB), TBB($99E7AFEABE000731), TBB($48CBFF086DDF285A),
  TBB($7F9B6AF1EBF78BAF), TBB($58627E1A149BBA21), TBB($2CD16E2ABD791E33), TBB($D363EFF5F0977996),
  TBB($0CE2A38C344A6EED), TBB($1A804AADB9CFA741), TBB($907F30421D78C5DE), TBB($501F65EDB3034D07),
  TBB($37624AE5A48FA6E9), TBB($957BAF61700CFF4E), TBB($3A6C27934E31188A), TBB($D49503536ABCA345),
  TBB($088E049589C432E0), TBB($F943AEE7FEBF21B8), TBB($6C3B8E3E336139D3), TBB($364F6FFA464EE52E),
  TBB($D60F6DCEDC314222), TBB($56963B0DCA418FC0), TBB($16F50EDF91E513AF), TBB($EF1955914B609F93),
  TBB($565601C0364E3228), TBB($ECB53939887E8175), TBB($BAC7A9A18531294B), TBB($B344C470397BBA52),
  TBB($65D34954DAF3CEBD), TBB($B4B81B3FA97511E2), TBB($B422061193D6F6A7), TBB($071582401C38434D),
  TBB($7A13F18BBEDC4FF5), TBB($BC4097B116C524D2), TBB($59B97885E2F2EA28), TBB($99170A5DC3115544),
  TBB($6F423357E7C6A9F9), TBB($325928EE6E6F8794), TBB($D0E4366228B03343), TBB($565C31F7DE89EA27),
  TBB($30F5611484119414), TBB($D873DB391292ED4F), TBB($7BD94E1D8E17DEBC), TBB($C7D9F16864A76E94),
  TBB($947AE053EE56E63C), TBB($C8C93882F9475F5F), TBB($3A9BF55BA91F81CA), TBB($D9A11FBB3D9808E4),
  TBB($0FD22063EDC29FCA), TBB($B3F256D8ACA0B0B9), TBB($B03031A8B4516E84), TBB($35DD37D5871448AF),
  TBB($E9F6082B05542E4E), TBB($EBFAFA33D7254B59), TBB($9255ABB50D532280), TBB($B9AB4CE57F2D34F3),
  TBB($693501D628297551), TBB($C62C58F97DD949BF), TBB($CD454F8F19C5126A), TBB($BBE83F4ECC2BDECB),
  TBB($DC842B7E2819E230), TBB($BA89142E007503B8), TBB($A3BC941D0A5061CB), TBB($E9F6760E32CD8021),
  TBB($09C7E552BC76492F), TBB($852F54934DA55CC9), TBB($8107FCCF064FCF56), TBB($098954D51FFF6580),
  TBB($23B70EDB1955C4BF), TBB($C330DE426430F69D), TBB($4715ED43E8A45C0A), TBB($A8D7E4DAB780A08D),
  TBB($0572B974F03CE0BB), TBB($B57D2E985E1419C7), TBB($E8D9ECBE2CF3D73F), TBB($2FE4B17170E59750),
  TBB($11317BA87905E790), TBB($7FBF21EC8A1F45EC), TBB($1725CABFCB045B00), TBB($964E915CD5E2B207),
  TBB($3E2B8BCBF016D66D), TBB($BE7444E39328A0AC), TBB($F85B2B4FBCDE44B7), TBB($49353FEA39BA63B1),
  TBB($1DD01AAFCD53486A), TBB($1FCA8A92FD719F85), TBB($FC7C95D827357AFA), TBB($18A6A990C8B35EBD),
  TBB($CCCB7005C6B9C28D), TBB($3BDBB92C43B17F26), TBB($AA70B5B4F89695A2), TBB($E94C39A54A98307F),
  TBB($B7A0B174CFF6F36E), TBB($D4DBA84729AF48AD), TBB($2E18BC1AD9704A68), TBB($2DE0966DAF2F8B1C),
  TBB($B9C11D5B1E43A07E), TBB($64972D68DEE33360), TBB($94628D38D0C20584), TBB($DBC0D2B6AB90A559),
  TBB($D2733C4335C6A72F), TBB($7E75D99D94A70F4D), TBB($6CED1983376FA72B), TBB($97FCAACBF030BC24),
  TBB($7B77497B32503B12), TBB($8547EDDFB81CCB94), TBB($79999CDFF70902CB), TBB($CFFE1939438E9B24),
  TBB($829626E3892D95D7), TBB($92FAE24291F2B3F1), TBB($63E22C147B9C3403), TBB($C678B6D860284A1C),
  TBB($5873888850659AE7), TBB($0981DCD296A8736D), TBB($9F65789A6509A440), TBB($9FF38FED72E9052F),
  TBB($E479EE5B9930578C), TBB($E7F28ECD2D49EECD), TBB($56C074A581EA17FE), TBB($5544F7D774B14AEF),
  TBB($7B3F0195FC6F290F), TBB($12153635B2C0CF57), TBB($7F5126DBBA5E0CA7), TBB($7A76956C3EAFB413),
  TBB($3D5774A11D31AB39), TBB($8A1B083821F40CB4), TBB($7B4A38E32537DF62), TBB($950113646D1D6E03),
  TBB($4DA8979A0041E8A9), TBB($3BC36E078F7515D7), TBB($5D0A12F27AD310D1), TBB($7F9D1A2E1EBE1327),
  TBB($DA3A361B1C5157B1), TBB($DCDD7D20903D0C25), TBB($36833336D068F707), TBB($CE68341F79893389),
  TBB($AB9090168DD05F34), TBB($43954B3252DC25E5), TBB($B438C2B67F98E5E9), TBB($10DCD78E3851A492),
  TBB($DBC27AB5447822BF), TBB($9B3CDB65F82CA382), TBB($B67B7896167B4C84), TBB($BFCED1B0048EAC50),
  TBB($A9119B60369FFEBD), TBB($1FFF7AC80904BF45), TBB($AC12FB171817EEE7), TBB($AF08DA9177DDA93D),
  TBB($1B0CAB936E65C744), TBB($B559EB1D04E5E932), TBB($C37B45B3F8D6F2BA), TBB($C3A9DC228CAAC9E9),
  TBB($F3B8B6675A6507FF), TBB($9FC477DE4ED681DA), TBB($67378D8ECCEF96CB), TBB($6DD856D94D259236),
  TBB($A319CE15B0B4DB31), TBB($073973751F12DD5E), TBB($8A8E849EB32781A5), TBB($E1925C71285279F5),
  TBB($74C04BF1790C0EFE), TBB($4DDA48153C94938A), TBB($9D266D6A1CC0542C), TBB($7440FB816508C4FE),
  TBB($13328503DF48229F), TBB($D6BF7BAEE43CAC40), TBB($4838D65F6EF6748F), TBB($1E152328F3318DEA),
  TBB($8F8419A348F296BF), TBB($72C8834A5957B511), TBB($D7A023A73260B45C), TBB($94EBC8ABCFB56DAE),
  TBB($9FC10D0F989993E0), TBB($DE68A2355B93CAE6), TBB($A44CFE79AE538BBE), TBB($9D1D84FCCE371425),
  TBB($51D2B1AB2DDFB636), TBB($2FD7E4B9E72CD38C), TBB($65CA5B96B7552210), TBB($DD69A0D8AB3B546D),
  TBB($604D51B25FBF70E2), TBB($73AA8A564FB7AC9E), TBB($1A8C1E992B941148), TBB($AAC40A2703D9BEA0),
  TBB($764DBEAE7FA4F3A6), TBB($1E99B96E70A9BE8B), TBB($2C5E9DEB57EF4743), TBB($3A938FEE32D29981),
  TBB($26E6DB8FFDF5ADFE), TBB($469356C504EC9F9D), TBB($C8763C5B08D1908C), TBB($3F6C6AF859D80055),
  TBB($7F7CC39420A3A545), TBB($9BFB227EBDF4C5CE), TBB($89039D79D6FC5C5C), TBB($8FE88B57305E2AB6),
  TBB($A09E8C8C35AB96DE), TBB($FA7E393983325753), TBB($D6B6D0ECC617C699), TBB($DFEA21EA9E7557E3),
  TBB($B67C1FA481680AF8), TBB($CA1E3785A9E724E5), TBB($1CFC8BED0D681639), TBB($D18D8549D140CAEA),
  TBB($4ED0FE7E9DC91335), TBB($E4DBF0634473F5D2), TBB($1761F93A44D5AEFE), TBB($53898E4C3910DA55),
  TBB($734DE8181F6EC39A), TBB($2680B122BAA28D97), TBB($298AF231C85BAFAB), TBB($7983EED3740847D5),
  TBB($66C1A2A1A60CD889), TBB($9E17E49642A3E4C1), TBB($EDB454E7BADC0805), TBB($50B704CAB602C329),
  TBB($4CC317FB9CDDD023), TBB($66B4835D9EAFEA22), TBB($219B97E26FFC81BD), TBB($261E4E4C0A333A9D),
  TBB($1FE2CCA76517DB90), TBB($D7504DFA8816EDBB), TBB($B9571FA04DC089C8), TBB($1DDC0325259B27DE),
  TBB($CF3F4688801EB9AA), TBB($F4F5D05C10CAB243), TBB($38B6525C21A42B0E), TBB($36F60E2BA4FA6800),
  TBB($EB3593803173E0CE), TBB($9C4CD6257C5A3603), TBB($AF0C317D32ADAA8A), TBB($258E5A80C7204C4B),
  TBB($8B889D624D44885D), TBB($F4D14597E660F855), TBB($D4347F66EC8941C3), TBB($E699ED85B0DFB40D),
  TBB($2472F6207C2D0484), TBB($C2A1E7B5B459AEB5), TBB($AB4F6451CC1D45EC), TBB($63767572AE3D6174),
  TBB($A59E0BD101731A28), TBB($116D0016CB948F09), TBB($2CF9C8CA052F6E9F), TBB($0B090A7560A968E3),
  TBB($ABEEDDB2DDE06FF1), TBB($58EFC10B06A2068D), TBB($C6E57A78FBD986E0), TBB($2EAB8CA63CE802D7),
  TBB($14A195640116F336), TBB($7C0828DD624EC390), TBB($D74BBE77E6116AC7), TBB($804456AF10F5FB53),
  TBB($EBE9EA2ADF4321C7), TBB($03219A39EE587A30), TBB($49787FEF17AF9924), TBB($A1E9300CD8520548),
  TBB($5B45E522E4B1B4EF), TBB($B49C3B3995091A36), TBB($D4490AD526F14431), TBB($12A8F216AF9418C2),
  TBB($001F837CC7350524), TBB($1877B51E57A764D5), TBB($A2853B80F17F58EE), TBB($993E1DE72D36D310),
  TBB($B3598080CE64A656), TBB($252F59CF0D9F04BB), TBB($D23C8E176D113600), TBB($1BDA0492E7E4586E),
  TBB($21E0BD5026C619BF), TBB($3B097ADAF088F94E), TBB($8D14DEDB30BE846E), TBB($F95CFFA23AF5F6F4),
  TBB($3871700761B3F743), TBB($CA672B91E9E4FA16), TBB($64C8E531BFF53B55), TBB($241260ED4AD1E87D),
  TBB($106C09B972D2E822), TBB($7FBA195410E5CA30), TBB($7884D9BC6CB569D8), TBB($0647DFEDCD894A29),
  TBB($63573FF03E224774), TBB($4FC8E9560F91B123), TBB($1DB956E450275779), TBB($B8D91274B9E9D4FB),
  TBB($A2EBEE47E2FBFCE1), TBB($D9F1F30CCD97FB09), TBB($EFED53D75FD64E6B), TBB($2E6D02C36017F67F),
  TBB($A9AA4D20DB084E9B), TBB($B64BE8D8B25396C1), TBB($70CB6AF7C2D5BCF0), TBB($98F076A4F7A2322E),
  TBB($BF84470805E69B5F), TBB($94C3251F06F90CF3), TBB($3E003E616A6591E9), TBB($B925A6CD0421AFF3),
  TBB($61BDD1307C66E300), TBB($BF8D5108E27E0D48), TBB($240AB57A8B888B20), TBB($FC87614BAF287E07),
  TBB($EF02CDD06FFDB432), TBB($A1082C0466DF6C0A), TBB($8215E577001332C8), TBB($D39BB9C3A48DB6CF),
  TBB($2738259634305C14), TBB($61CF4F94C97DF93D), TBB($1B6BACA2AE4E125B), TBB($758F450C88572E0B),
  TBB($959F587D507A8359), TBB($B063E962E045F54D), TBB($60E8ED72C0DFF5D1), TBB($7B64978555326F9F),
  TBB($FD080D236DA814BA), TBB($8C90FD9B083F4558), TBB($106F72FE81E2C590), TBB($7976033A39F7D952),
  TBB($A4EC0132764CA04B), TBB($733EA705FAE4FA77), TBB($B4D8F77BC3E56167), TBB($9E21F4F903B33FD9),
  TBB($9D765E419FB69F6D), TBB($D30C088BA61EA5EF), TBB($5D94337FBFAF7F5B), TBB($1A4E4822EB4D7A59),
  TBB($6FFE73E81B637FB3), TBB($DDF957BC36D8B9CA), TBB($64D0E29EEA8838B3), TBB($08DD9BDFD96B9F63),
  TBB($087E79E5A57D1D13), TBB($E328E230E3E2B3FB), TBB($1C2559E30F0946BE), TBB($720BF5F26F4D2EAA),
  TBB($B0774D261CC609DB), TBB($443F64EC5A371195), TBB($4112CF68649A260E), TBB($D813F2FAB7F5C5CA),
  TBB($660D3257380841EE), TBB($59AC2C7873F910A3), TBB($E846963877671A17), TBB($93B633ABFA3469F8),
  TBB($C0C0F5A60EF4CDCF), TBB($CAF21ECD4377B28C), TBB($57277707199B8175), TBB($506C11B9D90E8B1D),
  TBB($D83CC2687A19255F), TBB($4A29C6465A314CD1), TBB($ED2DF21216235097), TBB($B5635C95FF7296E2),
  TBB($22AF003AB672E811), TBB($52E762596BF68235), TBB($9AEBA33AC6ECC6B0), TBB($944F6DE09134DFB6),
  TBB($6C47BEC883A7DE39), TBB($6AD047C430A12104), TBB($A5B1CFDBA0AB4067), TBB($7C45D833AFF07862),
  TBB($5092EF950A16DA0B), TBB($9338E69C052B8E7B), TBB($455A4B4CFE30E3F5), TBB($6B02E63195AD0CF8),
  TBB($6B17B224BAD6BF27), TBB($D1E0CCD25BB9C169), TBB($DE0C89A556B9AE70), TBB($50065E535A213CF6),
  TBB($9C1169FA2777B874), TBB($78EDEFD694AF1EED), TBB($6DC93D9526A50E68), TBB($EE97F453F06791ED),
  TBB($32AB0EDB696703D3), TBB($3A6853C7E70757A7), TBB($31865CED6120F37D), TBB($67FEF95D92607890),
  TBB($1F2B1D1F15F6DC9C), TBB($B69E38A8965C6B65), TBB($AA9119FF184CCCF4), TBB($F43C732873F24C13),
  TBB($FB4A3D794A9A80D2), TBB($3550C2321FD6109C), TBB($371F77E76BB8417E), TBB($6BFA9AAE5EC05779),
  TBB($CD04F3FF001A4778), TBB($E3273522064480CA), TBB($9F91508BFFCFC14A), TBB($049A7F41061A9E60),
  TBB($FCB6BE43A9F2FE9B), TBB($08DE8A1C7797DA9B), TBB($8F9887E6078735A1), TBB($B5B4071DBFC73A66),
  TBB($230E343DFBA08D33), TBB($43ED7F5A0FAE657D), TBB($3A88A0FBBCB05C63), TBB($21874B8B4D2DBC4F),
  TBB($1BDEA12E35F6A8C9), TBB($53C065C6C8E63528), TBB($E34A1D250E7A8D6B), TBB($D6B04D3B7651DD7E),
  TBB($5E90277E7CB39E2D), TBB($2C046F22062DC67D), TBB($B10BB459132D0A26), TBB($3FA9DDFB67E2F199),
  TBB($0E09B88E1914F7AF), TBB($10E8B35AF3EEAB37), TBB($9EEDECA8E272B933), TBB($D4C718BC4AE8AE5F),
  TBB($81536D601170FC20), TBB($91B534F885818A06), TBB($EC8177F83F900978), TBB($190E714FADA5156E),
  TBB($B592BF39B0364963), TBB($89C350C893AE7DC1), TBB($AC042E70F8B383F2), TBB($B49B52E587A1EE60),
  TBB($FB152FE3FF26DA89), TBB($3E666E6F69AE2C15), TBB($3B544EBE544C19F9), TBB($E805A1E290CF2456),
  TBB($24B33C9D7ED25117), TBB($E74733427B72F0C1), TBB($0A804D18B7097475), TBB($57E3306D881EDB4F),
  TBB($4AE7D6A36EB5DBCB), TBB($2D8D5432157064C8), TBB($D1E649DE1E7F268B), TBB($8A328A1CEDFE552C),
  TBB($07A3AEC79624C7DA), TBB($84547DDC3E203C94), TBB($990A98FD5071D263), TBB($1A4FF12616EEFC89),
  TBB($F6F7FD1431714200), TBB($30C05B1BA332F41C), TBB($8D2636B81555A786), TBB($46C9FEB55D120902),
  TBB($CCEC0A73B49C9921), TBB($4E9D2827355FC492), TBB($19EBB029435DCB0F), TBB($4659D2B743848A2C),
  TBB($963EF2C96B33BE31), TBB($74F85198B05A2E7D), TBB($5A0F544DD2B1FB18), TBB($03727073C2E134B1),
  TBB($C7F6AA2DE59AEA61), TBB($352787BAA0D7C22F), TBB($9853EAB63B5E0B35), TBB($ABBDCDD7ED5C0860),
  TBB($CF05DAF5AC8D77B0), TBB($49CAD48CEBF4A71E), TBB($7A4C10EC2158C4A6), TBB($D9E92AA246BF719E),
  TBB($13AE978D09FE5557), TBB($730499AF921549FF), TBB($4E4B705B92903BA4), TBB($FF577222C14F0A3A),
  TBB($55B6344CF97AAFAE), TBB($B862225B055B6960), TBB($CAC09AFBDDD2CDB4), TBB($DAF8E9829FE96B5F),
  TBB($B5FDFC5D3132C498), TBB($310CB380DB6F7503), TBB($E87FBB46217A360E), TBB($2102AE466EBB1148),
  TBB($F8549E1A3AA5E00D), TBB($07A69AFDCC42261A), TBB($C4C118BFE78FEAAE), TBB($F9F4892ED96BD438),
  TBB($1AF3DBE25D8F45DA), TBB($F5B4B0B0D2DEEEB4), TBB($962ACEEFA82E1C84), TBB($046E3ECAAF453CE9),
  TBB($F05D129681949A4C), TBB($964781CE734B3C84), TBB($9C2ED44081CE5FBD), TBB($522E23F3925E319E),
  TBB($177E00F9FC32F791), TBB($2BC60A63A6F3B3F2), TBB($222BBFAE61725606), TBB($486289DDCC3D6780),
  TBB($7DC7785B8EFDFC80), TBB($8AF38731C02BA980), TBB($1FAB64EA29A2DDF7), TBB($E4D9429322CD065A),
  TBB($9DA058C67844F20C), TBB($24C0E332B70019B0), TBB($233003B5A6CFE6AD), TBB($D586BD01C5C217F6),
  TBB($5E5637885F29BC2B), TBB($7EBA726D8C94094B), TBB($0A56A5F0BFE39272), TBB($D79476A84EE20D06),
  TBB($9E4C1269BAA4BF37), TBB($17EFEE45B0DEE640), TBB($1D95B0A5FCF90BC6), TBB($93CBE0B699C2585D),
  TBB($65FA4F227A2B6D79), TBB($D5F9E858292504D5), TBB($C2B5A03F71471A6F), TBB($59300222B4561E00),
  TBB($CE2F8642CA0712DC), TBB($7CA9723FBB2E8988), TBB($2785338347F2BA08), TBB($C61BB3A141E50E8C),
  TBB($150F361DAB9DEC26), TBB($9F6A419D382595F4), TBB($64A53DC924FE7AC9), TBB($142DE49FFF7A7C3D),
  TBB($0C335248857FA9E7), TBB($0A9C32D5EAE45305), TBB($E6C42178C4BBB92E), TBB($71F1CE2490D20B07),
  TBB($F1BCC3D275AFE51A), TBB($E728E8C83C334074), TBB($96FBF83A12884624), TBB($81A1549FD6573DA5),
  TBB($5FA7867CAF35E149), TBB($56986E2EF3ED091B), TBB($917F1DD5F8886C61), TBB($D20D8C88C8FFE65F),
  TBB($31D71DCE64B2C310), TBB($F165B587DF898190), TBB($A57E6339DD2CF3A0), TBB($1EF6E6DBB1961EC9),
  TBB($70CC73D90BC26E24), TBB($E21A6B35DF0C3AD7), TBB($003A93D8B2806962), TBB($1C99DED33CB890A1),
  TBB($CF3145DE0ADD4289), TBB($D0E4427A5514FB72), TBB($77C621CC9FB3A483), TBB($67A34DAC4356550B),
  TBB($F8D626AAAF278509)
);

const
  PgPiece: array [TPiece] of integer =
    ( 0, 0, 1, 0,11,10, 3, 2, 5, 4, 7, 6, 9, 8, 0, 0);

  PgPromotion: array [0..4] of TPieceType =
    ( PT_NONE, KNIGHT, BISHOP, ROOK, QUEEN );

function pg_key_piece(p: TPiece; sq: TSquare): TKey; inline;
begin
  result := PgRandom[PgRandomPiece + PgPiece[p] * 64 + ord(sq)];
end;

function pg_key_ep(epsq: TSquare): TKey; inline;
begin
  if epsq <> NO_EP then
    result := PgRandom[PgRandomEP + ord(file_of(epsq))]
  else
    result := 0;
end;

function pg_key_stm(stm: TSide): TKey; inline;
begin
  if stm = WHITE then
    result := PgRandom[PgRandomTurn]
  else
    result := 0;
end;

function pg_key_castle(cr: TCastleRights): TKey; inline;
begin
  result := 0;
  if (cr and CR_W_SHORT <> 0) then result := result xor PgRandom[PgRandomCastle + 0];
  if (cr and CR_B_SHORT <> 0) then result := result xor PgRandom[PgRandomCastle + 2];
  if (cr and CR_W_LONG <> 0) then result := result xor PgRandom[PgRandomCastle + 1];
  if (cr and CR_B_LONG <> 0) then result := result xor PgRandom[PgRandomCastle + 3];
end;

