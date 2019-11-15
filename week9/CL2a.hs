import Text.Regex.Posix

authors = "Linda Fairstein (born 1947)\n\
 \Anthony Faramus (1920-1990)\n\
 \John Fante (1909-1983)\n\
 \Nuruddin Farah (born 1945)\n\
 \Nancy Farmer (born 1941)\n\
 \Penelope Farmer (born 1939)\n\
 \Philip Jose Farmer (born 1918)\n\
 \Howard Fast (1914-2003)\n\
 \Tarek Fatah (born 1949)\n\
 \William Faulkner (1897-1962)\n\
 \Madame de La Fayette (1634-1693)"
re :: String
re = "^((([A-Z][a-z]+) )+)" ++
  "(([a-z]+ ([A-Za-z]* )*)?[A-Z][a-z]*) \\((born )?([0-9]{4})(-[0-9]{4})?\\)$"
-- authors =~ re :: [[String]]  
gulliverstravels :: String
gulliverstravels =         "GULLIVER'S TRAVELS\n\
\                                INTO SEVERAL\n\
\                        REMOTE NATIONS OF THE WORLD\n\
\ \n\
\ \n\
\                          BY JONATHAN SWIFT, D.D.,\n\
\                       DEAN OF ST. PATRICK'S, DUBLIN.\n\
\ \n\
\                       [_First published in_ 1726-7.]\n\
\ \n\
\ \n\
\ \n\
\ \n\
\ THE PUBLISHER TO THE READER.\n\
\ \n\
\ \n\
\                   [_As given in the original edition_.]\n\
\ \n\
\ The author of these Travels, Mr. Lemuel Gulliver, is my ancient and\n\
\ intimate friend; there is likewise some relation between us on the\n\
\ mother's side.  About three years ago, Mr. Gulliver growing weary of the\n\
\ concourse of curious people coming to him at his house in Redriff, made a\n\
\ small purchase of land, with a convenient house, near Newark, in\n\
\ Nottinghamshire, his native country; where he now lives retired, yet in\n\
\ good esteem among his neighbours.\n\
\ \n\
\ Although Mr. Gulliver was born in Nottinghamshire, where his father\n\
\ dwelt, yet I have heard him say his family came from Oxfordshire; to\n\
\ confirm which, I have observed in the churchyard at Banbury in that\n\
\ county, several tombs and monuments of the Gullivers.\n\
\ \n\
\ Before he quitted Redriff, he left the custody of the following papers in\n\
\ my hands, with the liberty to dispose of them as I should think fit.  I\n\
\ have carefully perused them three times.  The style is very plain and\n\
\ simple; and the only fault I find is, that the author, after the manner\n\
\ of travellers, is a little too circumstantial.  There is an air of truth\n\
\ apparent through the whole; and indeed the author was so distinguished\n\
\ for his veracity, that it became a sort of proverb among his neighbours\n\
\ at Redriff, when any one affirmed a thing, to say, it was as true as if\n\
\ Mr. Gulliver had spoken it.\n\
\ \n\
\ By the advice of several worthy persons, to whom, with the author's\n\
\ permission, I communicated these papers, I now venture to send them into\n\
\ the world, hoping they may be, at least for some time, a better\n\
\ entertainment to our young noblemen, than the common scribbles of\n\
\ politics and party.\n\
\ \n\
\ This volume would have been at least twice as large, if I had not made\n\
\ bold to strike out innumerable passages relating to the winds and tides,\n\
\ as well as to the variations and bearings in the several voyages,\n\
\ together with the minute descriptions of the management of the ship in\n\
\ storms, in the style of sailors; likewise the account of longitudes and\n\
\ latitudes; wherein I have reason to apprehend, that Mr. Gulliver may be a\n\
\ little dissatisfied.  But I was resolved to fit the work as much as\n\
\ possible to the general capacity of readers.  However, if my own\n\
\ ignorance in sea affairs shall have led me to commit some mistakes, I\n\
\ alone am answerable for them.  And if any traveller hath a curiosity to\n\
\ see the whole work at large, as it came from the hands of the author, I\n\
\ will be ready to gratify him.\n\
\ \n\
\ As for any further particulars relating to the author, the reader will\n\
\ receive satisfaction from the first pages of the book.\n\
\ \n\
\                                                           RICHARD SYMPSON.\n\
\ \n\
\ \n\
\ \n\
\ \n\
\ A LETTER FROM CAPTAIN GULLIVER TO HIS COUSIN SYMPSON.\n\
\ \n\
\ \n\
\                         WRITTEN IN THE YEAR 1727.\n\
\ \n\
\ I hope you will be ready to own publicly, whenever you shall be called to\n\
\ it, that by your great and frequent urgency you prevailed on me to\n\
\ publish a very loose and uncorrect account of my travels, with directions\n\
\ to hire some young gentleman of either university to put them in order,\n\
\ and correct the style, as my cousin Dampier did, by my advice, in his\n\
\ book called \"A Voyage round the world.\"  But I do not remember I gave you\n\
\ power to consent that any thing should be omitted, and much less that any\n\
\ thing should be inserted; therefore, as to the latter, I do here renounce\n\
\ every thing of that kind; particularly a paragraph about her majesty\n\
\ Queen Anne, of most pious and glorious memory; although I did reverence\n\
\ and esteem her more than any of human species.  But you, or your\n\
\ interpolator, ought to have considered, that it was not my inclination,\n\
\ so was it not decent to praise any animal of our composition before my\n\
\ master _Houyhnhnm_: And besides, the fact was altogether false; for to my\n\
\ knowledge, being in England during some part of her majesty's reign, she\n\
\ did govern by a chief minister; nay even by two successively, the first\n\
\ whereof was the lord of Godolphin, and the second the lord of Oxford; so\n\
\ that you have made me say the thing that was not.  Likewise in the\n\
\ account of the academy of projectors, and several passages of my\n\
\ discourse to my master _Houyhnhnm_, you have either omitted some material\n\
\ circumstances, or minced or changed them in such a manner, that I do\n\
\ hardly know my own work.  When I formerly hinted to you something of this\n\
\ in a letter, you were pleased to answer that you were afraid of giving\n\
\ offence; that people in power were very watchful over the press, and apt\n\
\ not only to interpret, but to punish every thing which looked like an\n\
\ _innuendo_ (as I think you call it).  But, pray how could that which I\n\
\ spoke so many years ago, and at about five thousand leagues distance, in\n\
\ another reign, be applied to any of the _Yahoos_, who now are said to\n\
\ govern the herd; especially at a time when I little thought, or feared,\n\
\ the unhappiness of living under them?  Have not I the most reason to\n\
\ complain, when I see these very _Yahoos_ carried by _Houyhnhnms_ in a\n\
\ vehicle, as if they were brutes, and those the rational creatures?  And\n\
\ indeed to avoid so monstrous and detestable a sight was one principal\n\
\ motive of my retirement hither.\n\
\ \n\
\ Thus much I thought proper to tell you in relation to yourself, and to\n\
\ the trust I reposed in you.\n\
\ \n\
\ I do, in the next place, complain of my own great want of judgment, in\n\
\ being prevailed upon by the entreaties and false reasoning of you and\n\
\ some others, very much against my own opinion, to suffer my travels to be\n\
\ published.  Pray bring to your mind how often I desired you to consider,\n\
\ when you insisted on the motive of public good, that the _Yahoos_ were a\n\
\ species of animals utterly incapable of amendment by precept or example:\n\
\ and so it has proved; for, instead of seeing a full stop put to all\n\
\ abuses and corruptions, at least in this little island, as I had reason\n\
\ to expect; behold, after above six months warning, I cannot learn that my\n\
\ book has produced one single effect according to my intentions.  I\n\
\ desired you would let me know, by a letter, when party and faction were\n\
\ extinguished; judges learned and upright; pleaders honest and modest,\n\
\ with some tincture of common sense, and Smithfield blazing with pyramids\n\
\ of law books; the young nobility's education entirely changed; the\n\
\ physicians banished; the female _Yahoos_ abounding in virtue, honour,\n\
\ truth, and good sense; courts and levees of great ministers thoroughly\n\
\ weeded and swept; wit, merit, and learning rewarded; all disgracers of\n\
\ the press in prose and verse condemned to eat nothing but their own\n\
\ cotton, and quench their thirst with their own ink.  These, and a\n\
\ thousand other reformations, I firmly counted upon by your encouragement;\n\
\ as indeed they were plainly deducible from the precepts delivered in my\n\
\ book.  And it must be owned, that seven months were a sufficient time to\n\
\ correct every vice and folly to which _Yahoos_ are subject, if their\n\
\ natures had been capable of the least disposition to virtue or wisdom.\n\
\ Yet, so far have you been from answering my expectation in any of your\n\
\ letters; that on the contrary you are loading our carrier every week with\n\
\ libels, and keys, and reflections, and memoirs, and second parts; wherein\n\
\ I see myself accused of reflecting upon great state folk; of degrading\n\
\ human nature (for so they have still the confidence to style it), and of\n\
\ abusing the female sex.  I find likewise that the writers of those\n\
\ bundles are not agreed among themselves; for some of them will not allow\n\
\ me to be the author of my own travels; and others make me author of books\n\
\ to which I am wholly a stranger.\n\
\ \n\
\ I find likewise that your printer has been so careless as to confound the\n\
\ times, and mistake the dates, of my several voyages and returns; neither\n\
\ assigning the true year, nor the true month, nor day of the month: and I\n\
\ hear the original manuscript is all destroyed since the publication of my\n\
\ book; neither have I any copy left: however, I have sent you some\n\
\ corrections, which you may insert, if ever there should be a second\n\
\ edition: and yet I cannot stand to them; but shall leave that matter to\n\
\ my judicious and candid readers to adjust it as they please.\n\
\ \n\
\ I hear some of our sea _Yahoos_ find fault with my sea-language, as not\n\
\ proper in many parts, nor now in use.  I cannot help it.  In my first\n\
\ voyages, while I was young, I was instructed by the oldest mariners, and\n\
\ learned to speak as they did.  But I have since found that the sea\n\
\ _Yahoos_ are apt, like the land ones, to become new-fangled in their\n\
\ words, which the latter change every year; insomuch, as I remember upon\n\
\ each return to my own country their old dialect was so altered, that I\n\
\ could hardly understand the new.  And I observe, when any _Yahoo_ comes\n\
\ from London out of curiosity to visit me at my house, we neither of us\n\
\ are able to deliver our conceptions in a manner intelligible to the\n\
\ other.\n\
\ \n\
\ If the censure of the _Yahoos_ could any way affect me, I should have\n\
\ great reason to complain, that some of them are so bold as to think my\n\
\ book of travels a mere fiction out of mine own brain, and have gone so\n\
\ far as to drop hints, that the _Houyhnhnms_ and _Yahoos_ have no more\n\
\ existence than the inhabitants of Utopia.\n\
\ \n\
\ Indeed I must confess, that as to the people of _Lilliput_, _Brobdingrag_\n\
\ (for so the word should have been spelt, and not erroneously\n\
\ _Brobdingnag_), and _Laputa_, I have never yet heard of any _Yahoo_ so\n\
\ presumptuous as to dispute their being, or the facts I have related\n\
\ concerning them; because the truth immediately strikes every reader with\n\
\ conviction.  And is there less probability in my account of the\n\
\ _Houyhnhnms_ or _Yahoos_, when it is manifest as to the latter, there are\n\
\ so many thousands even in this country, who only differ from their\n\
\ brother brutes in _Houyhnhnmland_, because they use a sort of jabber, and\n\
\ do not go naked?  I wrote for their amendment, and not their approbation.\n\
\ The united praise of the whole race would be of less consequence to me,\n\
\ than the neighing of those two degenerate _Houyhnhnms_ I keep in my\n\
\ stable; because from these, degenerate as they are, I still improve in\n\
\ some virtues without any mixture of vice.\n\
\ \n\
\ Do these miserable animals presume to think, that I am so degenerated as\n\
\ to defend my veracity?  _Yahoo_ as I am, it is well known through all\n\
\ _Houyhnhnmland_, that, by the instructions and example of my illustrious\n\
\ master, I was able in the compass of two years (although I confess with\n\
\ the utmost difficulty) to remove that infernal habit of lying, shuffling,\n\
\ deceiving, and equivocating, so deeply rooted in the very souls of all my\n\
\ species; especially the Europeans.\n\
\ \n\
\ I have other complaints to make upon this vexatious occasion; but I\n\
\ forbear troubling myself or you any further.  I must freely confess, that\n\
\ since my last return, some corruptions of my _Yahoo_ nature have revived\n\
\ in me by conversing with a few of your species, and particularly those of\n\
\ my own family, by an unavoidable necessity; else I should never have\n\
\ attempted so absurd a project as that of reforming the _Yahoo_ race in\n\
\ this kingdom: But I have now done with all such visionary schemes for\n\
\ ever.\n\
\ \n\
\ _April_ 2, 1727\n\
\ \n\
\ \n\
\ \n\
\ \n\
\ PART I.  A VOYAGE TO LILLIPUT.\n\
\ \n\
\ \n\
\ CHAPTER I.\n\
\ \n\
\ \n\
\ The author gives some account of himself and family.  His first\n\
\ inducements to travel.  He is shipwrecked, and swims for his life.  Gets\n\
\ safe on shore in the country of Lilliput; is made a prisoner, and carried\n\
\ up the country.\n\
\ \n\
\ My father had a small estate in Nottinghamshire: I was the third of five\n\
\ sons.  He sent me to Emanuel College in Cambridge at fourteen years old,\n\
\ where I resided three years, and applied myself close to my studies; but\n\
\ the charge of maintaining me, although I had a very scanty allowance,\n\
\ being too great for a narrow fortune, I was bound apprentice to Mr. James\n\
\ Bates, an eminent surgeon in London, with whom I continued four years.\n\
\ My father now and then sending me small sums of money, I laid them out in\n\
\ learning navigation, and other parts of the mathematics, useful to those\n\
\ who intend to travel, as I always believed it would be, some time or\n\
\ other, my fortune to do.  When I left Mr. Bates, I went down to my\n\
\ father: where, by the assistance of him and my uncle John, and some other\n\
\ relations, I got forty pounds, and a promise of thirty pounds a year to\n\
\ maintain me at Leyden: there I studied physic two years and seven months,\n\
\ knowing it would be useful in long voyages.\n\
\ \n\
\ Soon after my return from Leyden, I was recommended by my good master,\n\
\ Mr. Bates, to be surgeon to the Swallow, Captain Abraham Pannel,\n\
\ commander; with whom I continued three years and a half, making a voyage\n\
\ or two into the Levant, and some other parts.  When I came back I\n\
\ resolved to settle in London; to which Mr. Bates, my master, encouraged\n\
\ me, and by him I was recommended to several patients.  I took part of a\n\
\ small house in the Old Jewry; and being advised to alter my condition, I\n\
\ married Mrs. Mary Burton, second daughter to Mr. Edmund Burton, hosier,\n\
\ in Newgate-street, with whom I received four hundred pounds for a\n\
\ portion.\n\
\ \n\
\ But my good master Bates dying in two years after, and I having few\n\
\ friends, my business began to fail; for my conscience would not suffer me\n\
\ to imitate the bad practice of too many among my brethren.  Having\n\
\ therefore consulted with my wife, and some of my acquaintance, I\n\
\ determined to go again to sea.  I was surgeon successively in two ships,\n\
\ and made several voyages, for six years, to the East and West Indies, by\n\
\ which I got some addition to my fortune.  My hours of leisure I spent in\n\
\ reading the best authors, ancient and modern, being always provided with\n\
\ a good number of books; and when I was ashore, in observing the manners\n\
\ and dispositions of the people, as well as learning their language;\n\
\ wherein I had a great facility, by the strength of my memory.\n\
\ \n\
\ The last of these voyages not proving very fortunate, I grew weary of the\n\
\ sea, and intended to stay at home with my wife and family.  I removed\n\
\ from the Old Jewry to Fetter Lane, and from thence to Wapping, hoping to\n\
\ get business among the sailors; but it would not turn to account.  After\n\
\ three years expectation that things would mend, I accepted an\n\
\ advantageous offer from Captain William Prichard, master of the Antelope,\n\
\ who was making a voyage to the South Sea.  We set sail from Bristol, May\n\
\ 4, 1699, and our voyage was at first very prosperous.\n\
\ \n\
\ It would not be proper, for some reasons, to trouble the reader with the\n\
\ particulars of our adventures in those seas; let it suffice to inform\n\
\ him, that in our passage from thence to the East Indies, we were driven\n\
\ by a violent storm to the north-west of Van Diemen's Land.  By an\n\
\ observation, we found ourselves in the latitude of 30 degrees 2 minutes\n\
\ south.  Twelve of our crew were dead by immoderate labour and ill food;\n\
\ the rest were in a very weak condition.  On the 5th of November, which\n\
\ was the beginning of summer in those parts, the weather being very hazy,\n\
\ the seamen spied a rock within half a cable's length of the ship; but the\n\
\ wind was so strong, that we were driven directly upon it, and immediately\n\
\ split.  Six of the crew, of whom I was one, having let down the boat into\n\
\ the sea, made a shift to get clear of the ship and the rock.  We rowed,\n\
\ by my computation, about three leagues, till we were able to work no\n\
\ longer, being already spent with labour while we were in the ship.  We\n\
\ therefore trusted ourselves to the mercy of the waves, and in about half\n\
\ an hour the boat was overset by a sudden flurry from the north.  What\n\
\ became of my companions in the boat, as well as of those who escaped on\n\
\ the rock, or were left in the vessel, I cannot tell; but conclude they\n\
\ were all lost.  For my own part, I swam as fortune directed me, and was\n\
\ pushed forward by wind and tide.  I often let my legs drop, and could\n\
\ feel no bottom; but when I was almost gone, and able to struggle no\n\
\ longer, I found myself within my depth; and by this time the storm was\n\
\ much abated.  The declivity was so small, that I walked near a mile\n\
\ before I got to the shore, which I conjectured was about eight o'clock in\n\
\ the evening.  I then advanced forward near half a mile, but could not\n\
\ discover any sign of houses or inhabitants; at least I was in so weak a\n\
\ condition, that I did not observe them.  I was extremely tired, and with\n\
\ that, and the heat of the weather, and about half a pint of brandy that I\n\
\ drank as I left the ship, I found myself much inclined to sleep.  I lay\n\
\ down on the grass, which was very short and soft, where I slept sounder\n\
\ than ever I remembered to have done in my life, and, as I reckoned, about\n\
\ nine hours; for when I awaked, it was just day-light.  I attempted to\n\
\ rise, but was not able to stir: for, as I happened to lie on my back, I\n\
\ found my arms and legs were strongly fastened on each side to the ground;\n\
\ and my hair, which was long and thick, tied down in the same manner.  I\n\
\ likewise felt several slender ligatures across my body, from my arm-pits\n\
\ to my thighs.  I could only look upwards; the sun began to grow hot, and\n\
\ the light offended my eyes.  I heard a confused noise about me; but in\n\
\ the posture I lay, could see nothing except the sky.  In a little time I\n\
\ felt something alive moving on my left leg, which advancing gently\n\
\ forward over my breast, came almost up to my chin; when, bending my eyes\n\
\ downwards as much as I could, I perceived it to be a human creature not\n\
\ six inches high, with a bow and arrow in his hands, and a quiver at his\n\
\ back.  In the mean time, I felt at least forty more of the same kind (as\n\
\ I conjectured) following the first.  I was in the utmost astonishment,\n\
\ and roared so loud, that they all ran back in a fright; and some of them,\n\
\ as I was afterwards told, were hurt with the falls they got by leaping\n\
\ from my sides upon the ground.  However, they soon returned, and one of\n\
\ them, who ventured so far as to get a full sight of my face, lifting up\n\
\ his hands and eyes by way of admiration, cried out in a shrill but\n\
\ distinct voice, _Hekinah degul_: the others repeated the same words\n\
\ several times, but then I knew not what they meant.  I lay all this\n\
\ while, as the reader may believe, in great uneasiness.  At length,\n\
\ struggling to get loose, I had the fortune to break the strings, and\n\
\ wrench out the pegs that fastened my left arm to the ground; for, by\n\
\ lifting it up to my face, I discovered the methods they had taken to bind\n\
\ me, and at the same time with a violent pull, which gave me excessive\n\
\ pain, I a little loosened the strings that tied down my hair on the left\n\
\ side, so that I was just able to turn my head about two inches.  But the\n\
\ creatures ran off a second time, before I could seize them; whereupon\n\
\ there was a great shout in a very shrill accent, and after it ceased I\n\
\ heard one of them cry aloud _Tolgo phonac_; when in an instant I felt\n\
\ above a hundred arrows discharged on my left hand, which, pricked me like\n\
\ so many needles; and besides, they shot another flight into the air, as\n\
\ we do bombs in Europe, whereof many, I suppose, fell on my body, (though\n\
\ I felt them not), and some on my face, which I immediately covered with\n\
\ my left hand.  When this shower of arrows was over, I fell a groaning\n\
\ with grief and pain; and then striving again to get loose, they\n\
\ discharged another volley larger than the first, and some of them\n\
\ attempted with spears to stick me in the sides; but by good luck I had on\n\
\ a buff jerkin, which they could not pierce.  I thought it the most\n\
\ prudent method to lie still, and my design was to continue so till night,\n\
\ when, my left hand being already loose, I could easily free myself: and\n\
\ as for the inhabitants, I had reason to believe I might be a match for\n\
\ the greatest army they could bring against me, if they were all of the\n\
\ same size with him that I saw.  But fortune disposed otherwise of me.\n\
\ When the people observed I was quiet, they discharged no more arrows;\n\
\ but, by the noise I heard, I knew their numbers increased; and about four\n\
\ yards from me, over against my right ear, I heard a knocking for above an\n\
\ hour, like that of people at work; when turning my head that way, as well\n\
\ as the pegs and strings would permit me, I saw a stage erected about a\n\
\ foot and a half from the ground, capable of holding four of the\n\
\ inhabitants, with two or three ladders to mount it: from whence one of\n\
\ them, who seemed to be a person of quality, made me a long speech,\n\
\ whereof I understood not one syllable.  But I should have mentioned, that\n\
\ before the principal person began his oration, he cried out three times,\n\
\ _Langro dehul san_ (these words and the former were afterwards repeated\n\
\ and explained to me); whereupon, immediately, about fifty of the\n\
\ inhabitants came and cut the strings that fastened the left side of my\n\
\ head, which gave me the liberty of turning it to the right, and of\n\
\ observing the person and gesture of him that was to speak.  He appeared\n\
\ to be of a middle age, and taller than any of the other three who\n\
\ attended him, whereof one was a page that held up his train, and seemed\n\
\ to be somewhat longer than my middle finger; the other two stood one on\n\
\ each side to support him.  He acted every part of an orator, and I could\n\
\ observe many periods of threatenings, and others of promises, pity, and\n\
\ kindness.  I answered in a few words, but in the most submissive manner,\n\
\ lifting up my left hand, and both my eyes to the sun, as calling him for\n\
\ a witness; and being almost famished with hunger, having not eaten a\n\
\ morsel for some hours before I left the ship, I found the demands of\n\
\ nature so strong upon me, that I could not forbear showing my impatience\n\
\ (perhaps against the strict rules of decency) by putting my finger\n\
\ frequently to my mouth, to signify that I wanted food.  The _hurgo_ (for\n\
\ so they call a great lord, as I afterwards learnt) understood me very\n\
\ well.  He descended from the stage, and commanded that several ladders\n\
\ should be applied to my sides, on which above a hundred of the\n\
\ inhabitants mounted and walked towards my mouth, laden with baskets full\n\
\ of meat, which had been provided and sent thither by the king's orders,\n\
\ upon the first intelligence he received of me.  I observed there was the\n\
\ flesh of several animals, but could not distinguish them by the taste.\n\
\ There were shoulders, legs, and loins, shaped like those of mutton, and\n\
\ very well dressed, but smaller than the wings of a lark.  I ate them by\n\
\ two or three at a mouthful, and took three loaves at a time, about the\n\
\ bigness of musket bullets.  They supplied me as fast as they could,\n\
\ showing a thousand marks of wonder and astonishment at my bulk and\n\
\ appetite.  I then made another sign, that I wanted drink.  They found by\n\
\ my eating that a small quantity would not suffice me; and being a most\n\
\ ingenious people, they slung up, with great dexterity, one of their\n\
\ largest hogsheads, then rolled it towards my hand, and beat out the top;\n\
\ I drank it off at a draught, which I might well do, for it did not hold\n\
\ half a pint, and tasted like a small wine of Burgundy, but much more\n\
\ delicious.  They brought me a second hogshead, which I drank in the same\n\
\ manner, and made signs for more; but they had none to give me.  When I\n\
\ had performed these wonders, they shouted for joy, and danced upon my\n\
\ breast, repeating several times as they did at first, _Hekinah degul_.\n\
\ They made me a sign that I should throw down the two hogsheads, but first\n\
\ warning the people below to stand out of the way, crying aloud, _Borach\n\
\ mevolah_; and when they saw the vessels in the air, there was a universal\n\
\ shout of _Hekinah degul_.  I confess I was often tempted, while they were\n\
\ passing backwards and forwards on my body, to seize forty or fifty of the\n\
\ first that came in my reach, and dash them against the ground.  But the\n\
\ remembrance of what I had felt, which probably might not be the worst\n\
\ they could do, and the promise of honour I made them--for so I\n\
\ interpreted my submissive behaviour--soon drove out these imaginations.\n\
\ Besides, I now considered myself as bound by the laws of hospitality, to\n\
\ a people who had treated me with so much expense and magnificence.\n\
\ However, in my thoughts I could not sufficiently wonder at the\n\
\ intrepidity of these diminutive mortals, who durst venture to mount and\n\
\ walk upon my body, while one of my hands was at liberty, without\n\
\ trembling at the very sight of so prodigious a creature as I must appear\n\
\ to them.  After some time, when they observed that I made no more demands\n\
\ for meat, there appeared before me a person of high rank from his\n\
\ imperial majesty.  His excellency, having mounted on the small of my\n\
\ right leg, advanced forwards up to my face, with about a dozen of his\n\
\ retinue; and producing his credentials under the signet royal, which he\n\
\ applied close to my eyes, spoke about ten minutes without any signs of\n\
\ anger, but with a kind of determinate resolution, often pointing\n\
\ forwards, which, as I afterwards found, was towards the capital city,\n\
\ about half a mile distant; whither it was agreed by his majesty in\n\
\ council that I must be conveyed.  I answered in few words, but to no\n\
\ purpose, and made a sign with my hand that was loose, putting it to the\n\
\ other (but over his excellency's head for fear of hurting him or his\n\
\ train) and then to my own head and body, to signify that I desired my\n\
\ liberty.  It appeared that he understood me well enough, for he shook his\n\
\ head by way of disapprobation, and held his hand in a posture to show\n\
\ that I must be carried as a prisoner.  However, he made other signs to\n\
\ let me understand that I should have meat and drink enough, and very good\n\
\ treatment.  Whereupon I once more thought of attempting to break my\n\
\ bonds; but again, when I felt the smart of their arrows upon my face and\n\
\ hands, which were all in blisters, and many of the darts still sticking\n\
\ in them, and observing likewise that the number of my enemies increased,\n\
\ I gave tokens to let them know that they might do with me what they\n\
\ pleased.  Upon this, the _hurgo_ and his train withdrew, with much\n\
\ civility and cheerful countenances.  Soon after I heard a general shout,\n\
\ with frequent repetitions of the words _Peplom selan_; and I felt great\n\
\ numbers of people on my left side relaxing the cords to such a degree,\n\
\ that I was able to turn upon my right, and to ease myself with making\n\
\ water; which I very plentifully did, to the great astonishment of the\n\
\ people; who, conjecturing by my motion what I was going to do,\n\
\ immediately opened to the right and left on that side, to avoid the\n\
\ torrent, which fell with such noise and violence from me.  But before\n\
\ this, they had daubed my face and both my hands with a sort of ointment,\n\
\ very pleasant to the smell, which, in a few minutes, removed all the\n\
\ smart of their arrows.  These circumstances, added to the refreshment I\n\
\ had received by their victuals and drink, which were very nourishing,\n\
\ disposed me to sleep.  I slept about eight hours, as I was afterwards\n\
\ assured; and it was no wonder, for the physicians, by the emperor's\n\
\ order, had mingled a sleepy potion in the hogsheads of wine.\n\
\ \n\
\ It seems, that upon the first moment I was discovered sleeping on the\n\
\ ground, after my landing, the emperor had early notice of it by an\n\
\ express; and determined in council, that I should be tied in the manner I\n\
\ have related, (which was done in the night while I slept;) that plenty of\n\
\ meat and drink should be sent to me, and a machine prepared to carry me\n\
\ to the capital city.\n\
\ \n\
\ This resolution perhaps may appear very bold and dangerous, and I am\n\
\ confident would not be imitated by any prince in Europe on the like\n\
\ occasion.  However, in my opinion, it was extremely prudent, as well as\n\
\ generous: for, supposing these people had endeavoured to kill me with\n\
\ their spears and arrows, while I was asleep, I should certainly have\n\
\ awaked with the first sense of smart, which might so far have roused my\n\
\ rage and strength, as to have enabled me to break the strings wherewith I\n\
\ was tied; after which, as they were not able to make resistance, so they\n\
\ could expect no mercy.\n\
\ \n\
\ These people are most excellent mathematicians, and arrived to a great\n\
\ perfection in mechanics, by the countenance and encouragement of the\n\
\ emperor, who is a renowned patron of learning.  This prince has several\n\
\ machines fixed on wheels, for the carriage of trees and other great\n\
\ weights.  He often builds his largest men of war, whereof some are nine\n\
\ feet long, in the woods where the timber grows, and has them carried on\n\
\ these engines three or four hundred yards to the sea.  Five hundred\n\
\ carpenters and engineers were immediately set at work to prepare the\n\
\ greatest engine they had.  It was a frame of wood raised three inches\n\
\ from the ground, about seven feet long, and four wide, moving upon\n\
\ twenty-two wheels.  The shout I heard was upon the arrival of this\n\
\ engine, which, it seems, set out in four hours after my landing.  It was\n\
\ brought parallel to me, as I lay.  But the principal difficulty was to\n\
\ raise and place me in this vehicle.  Eighty poles, each of one foot high,\n\
\ were erected for this purpose, and very strong cords, of the bigness of\n\
\ packthread, were fastened by hooks to many bandages, which the workmen\n\
\ had girt round my neck, my hands, my body, and my legs.  Nine hundred of\n\
\ the strongest men were employed to draw up these cords, by many pulleys\n\
\ fastened on the poles; and thus, in less than three hours, I was raised\n\
\ and slung into the engine, and there tied fast.  All this I was told;\n\
\ for, while the operation was performing, I lay in a profound sleep, by\n\
\ the force of that soporiferous medicine infused into my liquor.  Fifteen\n\
\ hundred of the emperor's largest horses, each about four inches and a\n\
\ half high, were employed to draw me towards the metropolis, which, as I\n\
\ said, was half a mile distant.\n\
\ \n\
\ About four hours after we began our journey, I awaked by a very\n\
\ ridiculous accident; for the carriage being stopped a while, to adjust\n\
\ something that was out of order, two or three of the young natives had\n\
\ the curiosity to see how I looked when I was asleep; they climbed up into\n\
\ the engine, and advancing very softly to my face, one of them, an officer\n\
\ in the guards, put the sharp end of his half-pike a good way up into my\n\
\ left nostril, which tickled my nose like a straw, and made me sneeze\n\
\ violently; whereupon they stole off unperceived, and it was three weeks\n\
\ before I knew the cause of my waking so suddenly.  We made a long march\n\
\ the remaining part of the day, and, rested at night with five hundred\n\
\ guards on each side of me, half with torches, and half with bows and\n\
\ arrows, ready to shoot me if I should offer to stir.  The next morning at\n\
\ sun-rise we continued our march, and arrived within two hundred yards of\n\
\ the city gates about noon.  The emperor, and all his court, came out to\n\
\ meet us; but his great officers would by no means suffer his majesty to\n\
\ endanger his person by mounting on my body.\n\
\ \n\
\ At the place where the carriage stopped there stood an ancient temple,\n\
\ esteemed to be the largest in the whole kingdom; which, having been\n\
\ polluted some years before by an unnatural murder, was, according to the\n\
\ zeal of those people, looked upon as profane, and therefore had been\n\
\ applied to common use, and all the ornaments and furniture carried away.\n\
\ In this edifice it was determined I should lodge.  The great gate\n\
\ fronting to the north was about four feet high, and almost two feet wide,\n\
\ through which I could easily creep.  On each side of the gate was a small\n\
\ window, not above six inches from the ground: into that on the left side,\n\
\ the king's smith conveyed fourscore and eleven chains, like those that\n\
\ hang to a lady's watch in Europe, and almost as large, which were locked\n\
\ to my left leg with six-and-thirty padlocks.  Over against this temple,\n\
\ on the other side of the great highway, at twenty feet distance, there\n\
\ was a turret at least five feet high.  Here the emperor ascended, with\n\
\ many principal lords of his court, to have an opportunity of viewing me,\n\
\ as I was told, for I could not see them.  It was reckoned that above a\n\
\ hundred thousand inhabitants came out of the town upon the same errand;\n\
\ and, in spite of my guards, I believe there could not be fewer than ten\n\
\ thousand at several times, who mounted my body by the help of ladders.\n\
\ But a proclamation was soon issued, to forbid it upon pain of death.\n\
\ When the workmen found it was impossible for me to break loose, they cut\n\
\ all the strings that bound me; whereupon I rose up, with as melancholy a\n\
\ disposition as ever I had in my life.  But the noise and astonishment of\n\
\ the people, at seeing me rise and walk, are not to be expressed.  The\n\
\ chains that held my left leg were about two yards long, and gave me not\n\
\ only the liberty of walking backwards and forwards in a semicircle, but,\n\
\ being fixed within four inches of the gate, allowed me to creep in, and\n\
\ lie at my full length in the temple.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER II.\n\
\ \n\
\ \n\
\ The emperor of Lilliput, attended by several of the nobility, comes to\n\
\ see the author in his confinement.  The emperor's person and habit\n\
\ described.  Learned men appointed to teach the author their language.  He\n\
\ gains favour by his mild disposition.  His pockets are searched, and his\n\
\ sword and pistols taken from him.\n\
\ \n\
\ When I found myself on my feet, I looked about me, and must confess I\n\
\ never beheld a more entertaining prospect.  The country around appeared\n\
\ like a continued garden, and the enclosed fields, which were generally\n\
\ forty feet square, resembled so many beds of flowers.  These fields were\n\
\ intermingled with woods of half a stang, {301} and the tallest trees, as\n\
\ I could judge, appeared to be seven feet high.  I viewed the town on my\n\
\ left hand, which looked like the painted scene of a city in a theatre.\n\
\ \n\
\ I had been for some hours extremely pressed by the necessities of nature;\n\
\ which was no wonder, it being almost two days since I had last\n\
\ disburdened myself.  I was under great difficulties between urgency and\n\
\ shame.  The best expedient I could think of, was to creep into my house,\n\
\ which I accordingly did; and shutting the gate after me, I went as far as\n\
\ the length of my chain would suffer, and discharged my body of that\n\
\ uneasy load.  But this was the only time I was ever guilty of so\n\
\ uncleanly an action; for which I cannot but hope the candid reader will\n\
\ give some allowance, after he has maturely and impartially considered my\n\
\ case, and the distress I was in.  From this time my constant practice\n\
\ was, as soon as I rose, to perform that business in open air, at the full\n\
\ extent of my chain; and due care was taken every morning before company\n\
\ came, that the offensive matter should be carried off in wheel-barrows,\n\
\ by two servants appointed for that purpose.  I would not have dwelt so\n\
\ long upon a circumstance that, perhaps, at first sight, may appear not\n\
\ very momentous, if I had not thought it necessary to justify my\n\
\ character, in point of cleanliness, to the world; which, I am told, some\n\
\ of my maligners have been pleased, upon this and other occasions, to call\n\
\ in question.\n\
\ \n\
\ When this adventure was at an end, I came back out of my house, having\n\
\ occasion for fresh air.  The emperor was already descended from the\n\
\ tower, and advancing on horseback towards me, which had like to have cost\n\
\ him dear; for the beast, though very well trained, yet wholly unused to\n\
\ such a sight, which appeared as if a mountain moved before him, reared up\n\
\ on its hinder feet: but that prince, who is an excellent horseman, kept\n\
\ his seat, till his attendants ran in, and held the bridle, while his\n\
\ majesty had time to dismount.  When he alighted, he surveyed me round\n\
\ with great admiration; but kept beyond the length of my chain.  He\n\
\ ordered his cooks and butlers, who were already prepared, to give me\n\
\ victuals and drink, which they pushed forward in a sort of vehicles upon\n\
\ wheels, till I could reach them.  I took these vehicles and soon emptied\n\
\ them all; twenty of them were filled with meat, and ten with liquor; each\n\
\ of the former afforded me two or three good mouthfuls; and I emptied the\n\
\ liquor of ten vessels, which was contained in earthen vials, into one\n\
\ vehicle, drinking it off at a draught; and so I did with the rest.  The\n\
\ empress, and young princes of the blood of both sexes, attended by many\n\
\ ladies, sat at some distance in their chairs; but upon the accident that\n\
\ happened to the emperor's horse, they alighted, and came near his person,\n\
\ which I am now going to describe.  He is taller by almost the breadth of\n\
\ my nail, than any of his court; which alone is enough to strike an awe\n\
\ into the beholders.  His features are strong and masculine, with an\n\
\ Austrian lip and arched nose, his complexion olive, his countenance\n\
\ erect, his body and limbs well proportioned, all his motions graceful,\n\
\ and his deportment majestic.  He was then past his prime, being\n\
\ twenty-eight years and three quarters old, of which he had reigned about\n\
\ seven in great felicity, and generally victorious.  For the better\n\
\ convenience of beholding him, I lay on my side, so that my face was\n\
\ parallel to his, and he stood but three yards off: however, I have had\n\
\ him since many times in my hand, and therefore cannot be deceived in the\n\
\ description.  His dress was very plain and simple, and the fashion of it\n\
\ between the Asiatic and the European; but he had on his head a light\n\
\ helmet of gold, adorned with jewels, and a plume on the crest.  He held\n\
\ his sword drawn in his hand to defend himself, if I should happen to\n\
\ break loose; it was almost three inches long; the hilt and scabbard were\n\
\ gold enriched with diamonds.  His voice was shrill, but very clear and\n\
\ articulate; and I could distinctly hear it when I stood up.  The ladies\n\
\ and courtiers were all most magnificently clad; so that the spot they\n\
\ stood upon seemed to resemble a petticoat spread upon the ground,\n\
\ embroidered with figures of gold and silver.  His imperial majesty spoke\n\
\ often to me, and I returned answers: but neither of us could understand a\n\
\ syllable.  There were several of his priests and lawyers present (as I\n\
\ conjectured by their habits), who were commanded to address themselves to\n\
\ me; and I spoke to them in as many languages as I had the least\n\
\ smattering of, which were High and Low Dutch, Latin, French, Spanish,\n\
\ Italian, and Lingua Franca, but all to no purpose.  After about two hours\n\
\ the court retired, and I was left with a strong guard, to prevent the\n\
\ impertinence, and probably the malice of the rabble, who were very\n\
\ impatient to crowd about me as near as they durst; and some of them had\n\
\ the impudence to shoot their arrows at me, as I sat on the ground by the\n\
\ door of my house, whereof one very narrowly missed my left eye.  But the\n\
\ colonel ordered six of the ringleaders to be seized, and thought no\n\
\ punishment so proper as to deliver them bound into my hands; which some\n\
\ of his soldiers accordingly did, pushing them forward with the butt-ends\n\
\ of their pikes into my reach.  I took them all in my right hand, put five\n\
\ of them into my coat-pocket; and as to the sixth, I made a countenance as\n\
\ if I would eat him alive.  The poor man squalled terribly, and the\n\
\ colonel and his officers were in much pain, especially when they saw me\n\
\ take out my penknife: but I soon put them out of fear; for, looking\n\
\ mildly, and immediately cutting the strings he was bound with, I set him\n\
\ gently on the ground, and away he ran.  I treated the rest in the same\n\
\ manner, taking them one by one out of my pocket; and I observed both the\n\
\ soldiers and people were highly delighted at this mark of my clemency,\n\
\ which was represented very much to my advantage at court.\n\
\ \n\
\ Towards night I got with some difficulty into my house, where I lay on\n\
\ the ground, and continued to do so about a fortnight; during which time,\n\
\ the emperor gave orders to have a bed prepared for me.  Six hundred beds\n\
\ of the common measure were brought in carriages, and worked up in my\n\
\ house; a hundred and fifty of their beds, sewn together, made up the\n\
\ breadth and length; and these were four double: which, however, kept me\n\
\ but very indifferently from the hardness of the floor, that was of smooth\n\
\ stone.  By the same computation, they provided me with sheets, blankets,\n\
\ and coverlets, tolerable enough for one who had been so long inured to\n\
\ hardships.\n\
\ \n\
\ As the news of my arrival spread through the kingdom, it brought\n\
\ prodigious numbers of rich, idle, and curious people to see me; so that\n\
\ the villages were almost emptied; and great neglect of tillage and\n\
\ household affairs must have ensued, if his imperial majesty had not\n\
\ provided, by several proclamations and orders of state, against this\n\
\ inconveniency.  He directed that those who had already beheld me should\n\
\ return home, and not presume to come within fifty yards of my house,\n\
\ without license from the court; whereby the secretaries of state got\n\
\ considerable fees.\n\
\ \n\
\ In the mean time the emperor held frequent councils, to debate what\n\
\ course should be taken with me; and I was afterwards assured by a\n\
\ particular friend, a person of great quality, who was as much in the\n\
\ secret as any, that the court was under many difficulties concerning me.\n\
\ They apprehended my breaking loose; that my diet would be very expensive,\n\
\ and might cause a famine.  Sometimes they determined to starve me; or at\n\
\ least to shoot me in the face and hands with poisoned arrows, which would\n\
\ soon despatch me; but again they considered, that the stench of so large\n\
\ a carcass might produce a plague in the metropolis, and probably spread\n\
\ through the whole kingdom.  In the midst of these consultations, several\n\
\ officers of the army went to the door of the great council-chamber, and\n\
\ two of them being admitted, gave an account of my behaviour to the six\n\
\ criminals above-mentioned; which made so favourable an impression in the\n\
\ breast of his majesty and the whole board, in my behalf, that an imperial\n\
\ commission was issued out, obliging all the villages, nine hundred yards\n\
\ round the city, to deliver in every morning six beeves, forty sheep, and\n\
\ other victuals for my sustenance; together with a proportionable quantity\n\
\ of bread, and wine, and other liquors; for the due payment of which, his\n\
\ majesty gave assignments upon his treasury:--for this prince lives\n\
\ chiefly upon his own demesnes; seldom, except upon great occasions,\n\
\ raising any subsidies upon his subjects, who are bound to attend him in\n\
\ his wars at their own expense.  An establishment was also made of six\n\
\ hundred persons to be my domestics, who had board-wages allowed for their\n\
\ maintenance, and tents built for them very conveniently on each side of\n\
\ my door.  It was likewise ordered, that three hundred tailors should make\n\
\ me a suit of clothes, after the fashion of the country; that six of his\n\
\ majesty's greatest scholars should be employed to instruct me in their\n\
\ language; and lastly, that the emperor's horses, and those of the\n\
\ nobility and troops of guards, should be frequently exercised in my\n\
\ sight, to accustom themselves to me.  All these orders were duly put in\n\
\ execution; and in about three weeks I made a great progress in learning\n\
\ their language; during which time the emperor frequently honoured me with\n\
\ his visits, and was pleased to assist my masters in teaching me.  We\n\
\ began already to converse together in some sort; and the first words I\n\
\ learnt, were to express my desire \"that he would please give me my\n\
\ liberty;\" which I every day repeated on my knees.  His answer, as I could\n\
\ comprehend it, was, \"that this must be a work of time, not to be thought\n\
\ on without the advice of his council, and that first I must _lumos kelmin\n\
\ pesso desmar lon emposo_;\" that is, swear a peace with him and his\n\
\ kingdom.  However, that I should be used with all kindness.  And he\n\
\ advised me to \"acquire, by my patience and discreet behaviour, the good\n\
\ opinion of himself and his subjects.\"  He desired \"I would not take it\n\
\ ill, if he gave orders to certain proper officers to search me; for\n\
\ probably I might carry about me several weapons, which must needs be\n\
\ dangerous things, if they answered the bulk of so prodigious a person.\"\n\
\ I said, \"His majesty should be satisfied; for I was ready to strip\n\
\ myself, and turn up my pockets before him.\"  This I delivered part in\n\
\ words, and part in signs.  He replied, \"that, by the laws of the kingdom,\n\
\ I must be searched by two of his officers; that he knew this could not be\n\
\ done without my consent and assistance; and he had so good an opinion of\n\
\ my generosity and justice, as to trust their persons in my hands; that\n\
\ whatever they took from me, should be returned when I left the country,\n\
\ or paid for at the rate which I would set upon them.\"  I took up the two\n\
\ officers in my hands, put them first into my coat-pockets, and then into\n\
\ every other pocket about me, except my two fobs, and another secret\n\
\ pocket, which I had no mind should be searched, wherein I had some little\n\
\ necessaries that were of no consequence to any but myself.  In one of my\n\
\ fobs there was a silver watch, and in the other a small quantity of gold\n\
\ in a purse.  These gentlemen, having pen, ink, and paper, about them,\n\
\ made an exact inventory of every thing they saw; and when they had done,\n\
\ desired I would set them down, that they might deliver it to the emperor.\n\
\ This inventory I afterwards translated into English, and is, word for\n\
\ word, as follows:\n\
\ \n\
\     \"_Imprimis_: In the right coat-pocket of the great man-mountain\" (for\n\
\     so I interpret the words _quinbus flestrin_,) \"after the strictest\n\
\     search, we found only one great piece of coarse-cloth, large enough\n\
\     to be a foot-cloth for your majesty's chief room of state.  In the\n\
\     left pocket we saw a huge silver chest, with a cover of the same\n\
\     metal, which we, the searchers, were not able to lift.  We desired it\n\
\     should be opened, and one of us stepping into it, found himself up to\n\
\     the mid leg in a sort of dust, some part whereof flying up to our\n\
\     faces set us both a sneezing for several times together.  In his\n\
\     right waistcoat-pocket we found a prodigious bundle of white thin\n\
\     substances, folded one over another, about the bigness of three men,\n\
\     tied with a strong cable, and marked with black figures; which we\n\
\     humbly conceive to be writings, every letter almost half as large as\n\
\     the palm of our hands.  In the left there was a sort of engine, from\n\
\     the back of which were extended twenty long poles, resembling the\n\
\     pallisados before your majesty's court: wherewith we conjecture the\n\
\     man-mountain combs his head; for we did not always trouble him with\n\
\     questions, because we found it a great difficulty to make him\n\
\     understand us.  In the large pocket, on the right side of his middle\n\
\     cover\" (so I translate the word _ranfulo_, by which they meant my\n\
\     breeches,) \"we saw a hollow pillar of iron, about the length of a\n\
\     man, fastened to a strong piece of timber larger than the pillar; and\n\
\     upon one side of the pillar, were huge pieces of iron sticking out,\n\
\     cut into strange figures, which we know not what to make of.  In the\n\
\     left pocket, another engine of the same kind.  In the smaller pocket\n\
\     on the right side, were several round flat pieces of white and red\n\
\     metal, of different bulk; some of the white, which seemed to be\n\
\     silver, were so large and heavy, that my comrade and I could hardly\n\
\     lift them.  In the left pocket were two black pillars irregularly\n\
\     shaped: we could not, without difficulty, reach the top of them, as\n\
\     we stood at the bottom of his pocket.  One of them was covered, and\n\
\     seemed all of a piece: but at the upper end of the other there\n\
\     appeared a white round substance, about twice the bigness of our\n\
\     heads.  Within each of these was enclosed a prodigious plate of\n\
\     steel; which, by our orders, we obliged him to show us, because we\n\
\     apprehended they might be dangerous engines.  He took them out of\n\
\     their cases, and told us, that in his own country his practice was to\n\
\     shave his beard with one of these, and cut his meat with the other.\n\
\     There were two pockets which we could not enter: these he called his\n\
\     fobs; they were two large slits cut into the top of his middle cover,\n\
\     but squeezed close by the pressure of his belly.  Out of the right\n\
\     fob hung a great silver chain, with a wonderful kind of engine at the\n\
\     bottom.  We directed him to draw out whatever was at the end of that\n\
\     chain; which appeared to be a globe, half silver, and half of some\n\
\     transparent metal; for, on the transparent side, we saw certain\n\
\     strange figures circularly drawn, and thought we could touch them,\n\
\     till we found our fingers stopped by the lucid substance.  He put\n\
\     this engine into our ears, which made an incessant noise, like that\n\
\     of a water-mill: and we conjecture it is either some unknown animal,\n\
\     or the god that he worships; but we are more inclined to the latter\n\
\     opinion, because he assured us, (if we understood him right, for he\n\
\     expressed himself very imperfectly) that he seldom did any thing\n\
\     without consulting it.  He called it his oracle, and said, it pointed\n\
\     out the time for every action of his life.  From the left fob he took\n\
\     out a net almost large enough for a fisherman, but contrived to open\n\
\     and shut like a purse, and served him for the same use: we found\n\
\     therein several massy pieces of yellow metal, which, if they be real\n\
\     gold, must be of immense value.\n\
\ \n\
\     \"Having thus, in obedience to your majesty's commands, diligently\n\
\     searched all his pockets, we observed a girdle about his waist made\n\
\     of the hide of some prodigious animal, from which, on the left side,\n\
\     hung a sword of the length of five men; and on the right, a bag or\n\
\     pouch divided into two cells, each cell capable of holding three of\n\
\     your majesty's subjects.  In one of these cells were several globes,\n\
\     or balls, of a most ponderous metal, about the bigness of our heads,\n\
\     and requiring a strong hand to lift them: the other cell contained a\n\
\     heap of certain black grains, but of no great bulk or weight, for we\n\
\     could hold above fifty of them in the palms of our hands.\n\
\ \n\
\     \"This is an exact inventory of what we found about the body of the\n\
\     man-mountain, who used us with great civility, and due respect to\n\
\     your majesty's commission.  Signed and sealed on the fourth day of\n\
\     the eighty-ninth moon of your majesty's auspicious reign.\n\
\ \n\
\                                           CLEFRIN FRELOCK, MARSI FRELOCK.\"\n\
\ \n\
\ When this inventory was read over to the emperor, he directed me,\n\
\ although in very gentle terms, to deliver up the several particulars.  He\n\
\ first called for my scimitar, which I took out, scabbard and all.  In the\n\
\ mean time he ordered three thousand of his choicest troops (who then\n\
\ attended him) to surround me at a distance, with their bows and arrows\n\
\ just ready to discharge; but I did not observe it, for mine eyes were\n\
\ wholly fixed upon his majesty.  He then desired me to draw my scimitar,\n\
\ which, although it had got some rust by the sea water, was, in most\n\
\ parts, exceeding bright.  I did so, and immediately all the troops gave a\n\
\ shout between terror and surprise; for the sun shone clear, and the\n\
\ reflection dazzled their eyes, as I waved the scimitar to and fro in my\n\
\ hand.  His majesty, who is a most magnanimous prince, was less daunted\n\
\ than I could expect: he ordered me to return it into the scabbard, and\n\
\ cast it on the ground as gently as I could, about six feet from the end\n\
\ of my chain.  The next thing he demanded was one of the hollow iron\n\
\ pillars; by which he meant my pocket pistols.  I drew it out, and at his\n\
\ desire, as well as I could, expressed to him the use of it; and charging\n\
\ it only with powder, which, by the closeness of my pouch, happened to\n\
\ escape wetting in the sea (an inconvenience against which all prudent\n\
\ mariners take special care to provide,) I first cautioned the emperor not\n\
\ to be afraid, and then I let it off in the air.  The astonishment here\n\
\ was much greater than at the sight of my scimitar.  Hundreds fell down as\n\
\ if they had been struck dead; and even the emperor, although he stood his\n\
\ ground, could not recover himself for some time.  I delivered up both my\n\
\ pistols in the same manner as I had done my scimitar, and then my pouch\n\
\ of powder and bullets; begging him that the former might be kept from\n\
\ fire, for it would kindle with the smallest spark, and blow up his\n\
\ imperial palace into the air.  I likewise delivered up my watch, which\n\
\ the emperor was very curious to see, and commanded two of his tallest\n\
\ yeomen of the guards to bear it on a pole upon their shoulders, as\n\
\ draymen in England do a barrel of ale.  He was amazed at the continual\n\
\ noise it made, and the motion of the minute-hand, which he could easily\n\
\ discern; for their sight is much more acute than ours: he asked the\n\
\ opinions of his learned men about it, which were various and remote, as\n\
\ the reader may well imagine without my repeating; although indeed I could\n\
\ not very perfectly understand them.  I then gave up my silver and copper\n\
\ money, my purse, with nine large pieces of gold, and some smaller ones;\n\
\ my knife and razor, my comb and silver snuff-box, my handkerchief and\n\
\ journal-book.  My scimitar, pistols, and pouch, were conveyed in\n\
\ carriages to his majesty's stores; but the rest of my goods were returned\n\
\ me.\n\
\ \n\
\ I had as I before observed, one private pocket, which escaped their\n\
\ search, wherein there was a pair of spectacles (which I sometimes use for\n\
\ the weakness of mine eyes,) a pocket perspective, and some other little\n\
\ conveniences; which, being of no consequence to the emperor, I did not\n\
\ think myself bound in honour to discover, and I apprehended they might be\n\
\ lost or spoiled if I ventured them out of my possession.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER III.\n\
\ \n\
\ \n\
\ The author diverts the emperor, and his nobility of both sexes, in a very\n\
\ uncommon manner.  The diversions of the court of Lilliput described.  The\n\
\ author has his liberty granted him upon certain conditions.\n\
\ \n\
\ My gentleness and good behaviour had gained so far on the emperor and his\n\
\ court, and indeed upon the army and people in general, that I began to\n\
\ conceive hopes of getting my liberty in a short time.  I took all\n\
\ possible methods to cultivate this favourable disposition.  The natives\n\
\ came, by degrees, to be less apprehensive of any danger from me.  I would\n\
\ sometimes lie down, and let five or six of them dance on my hand; and at\n\
\ last the boys and girls would venture to come and play at hide-and-seek\n\
\ in my hair.  I had now made a good progress in understanding and speaking\n\
\ the language.  The emperor had a mind one day to entertain me with\n\
\ several of the country shows, wherein they exceed all nations I have\n\
\ known, both for dexterity and magnificence.  I was diverted with none so\n\
\ much as that of the rope-dancers, performed upon a slender white thread,\n\
\ extended about two feet, and twelve inches from the ground.  Upon which I\n\
\ shall desire liberty, with the reader's patience, to enlarge a little.\n\
\ \n\
\ This diversion is only practised by those persons who are candidates for\n\
\ great employments, and high favour at court.  They are trained in this\n\
\ art from their youth, and are not always of noble birth, or liberal\n\
\ education.  When a great office is vacant, either by death or disgrace\n\
\ (which often happens,) five or six of those candidates petition the\n\
\ emperor to entertain his majesty and the court with a dance on the rope;\n\
\ and whoever jumps the highest, without falling, succeeds in the office.\n\
\ Very often the chief ministers themselves are commanded to show their\n\
\ skill, and to convince the emperor that they have not lost their faculty.\n\
\ Flimnap, the treasurer, is allowed to cut a caper on the straight rope,\n\
\ at least an inch higher than any other lord in the whole empire.  I have\n\
\ seen him do the summerset several times together, upon a trencher fixed\n\
\ on a rope which is no thicker than a common packthread in England.  My\n\
\ friend Reldresal, principal secretary for private affairs, is, in my\n\
\ opinion, if I am not partial, the second after the treasurer; the rest of\n\
\ the great officers are much upon a par.\n\
\ \n\
\ These diversions are often attended with fatal accidents, whereof great\n\
\ numbers are on record.  I myself have seen two or three candidates break\n\
\ a limb.  But the danger is much greater, when the ministers themselves\n\
\ are commanded to show their dexterity; for, by contending to excel\n\
\ themselves and their fellows, they strain so far that there is hardly one\n\
\ of them who has not received a fall, and some of them two or three.  I\n\
\ was assured that, a year or two before my arrival, Flimnap would\n\
\ infallibly have broke his neck, if one of the king's cushions, that\n\
\ accidentally lay on the ground, had not weakened the force of his fall.\n\
\ \n\
\ There is likewise another diversion, which is only shown before the\n\
\ emperor and empress, and first minister, upon particular occasions.  The\n\
\ emperor lays on the table three fine silken threads of six inches long;\n\
\ one is blue, the other red, and the third green.  These threads are\n\
\ proposed as prizes for those persons whom the emperor has a mind to\n\
\ distinguish by a peculiar mark of his favour.  The ceremony is performed\n\
\ in his majesty's great chamber of state, where the candidates are to\n\
\ undergo a trial of dexterity very different from the former, and such as\n\
\ I have not observed the least resemblance of in any other country of the\n\
\ new or old world.  The emperor holds a stick in his hands, both ends\n\
\ parallel to the horizon, while the candidates advancing, one by one,\n\
\ sometimes leap over the stick, sometimes creep under it, backward and\n\
\ forward, several times, according as the stick is advanced or depressed.\n\
\ Sometimes the emperor holds one end of the stick, and his first minister\n\
\ the other; sometimes the minister has it entirely to himself.  Whoever\n\
\ performs his part with most agility, and holds out the longest in leaping\n\
\ and creeping, is rewarded with the blue-coloured silk; the red is given\n\
\ to the next, and the green to the third, which they all wear girt twice\n\
\ round about the middle; and you see few great persons about this court\n\
\ who are not adorned with one of these girdles.\n\
\ \n\
\ The horses of the army, and those of the royal stables, having been daily\n\
\ led before me, were no longer shy, but would come up to my very feet\n\
\ without starting.  The riders would leap them over my hand, as I held it\n\
\ on the ground; and one of the emperor's huntsmen, upon a large courser,\n\
\ took my foot, shoe and all; which was indeed a prodigious leap.  I had\n\
\ the good fortune to divert the emperor one day after a very extraordinary\n\
\ manner.  I desired he would order several sticks of two feet high, and\n\
\ the thickness of an ordinary cane, to be brought me; whereupon his\n\
\ majesty commanded the master of his woods to give directions accordingly;\n\
\ and the next morning six woodmen arrived with as many carriages, drawn by\n\
\ eight horses to each.  I took nine of these sticks, and fixing them\n\
\ firmly in the ground in a quadrangular figure, two feet and a half\n\
\ square, I took four other sticks, and tied them parallel at each corner,\n\
\ about two feet from the ground; then I fastened my handkerchief to the\n\
\ nine sticks that stood erect; and extended it on all sides, till it was\n\
\ tight as the top of a drum; and the four parallel sticks, rising about\n\
\ five inches higher than the handkerchief, served as ledges on each side.\n\
\ When I had finished my work, I desired the emperor to let a troop of his\n\
\ best horses twenty-four in number, come and exercise upon this plain.\n\
\ His majesty approved of the proposal, and I took them up, one by one, in\n\
\ my hands, ready mounted and armed, with the proper officers to exercise\n\
\ them.  As soon as they got into order they divided into two parties,\n\
\ performed mock skirmishes, discharged blunt arrows, drew their swords,\n\
\ fled and pursued, attacked and retired, and in short discovered the best\n\
\ military discipline I ever beheld.  The parallel sticks secured them and\n\
\ their horses from falling over the stage; and the emperor was so much\n\
\ delighted, that he ordered this entertainment to be repeated several\n\
\ days, and once was pleased to be lifted up and give the word of command;\n\
\ and with great difficulty persuaded even the empress herself to let me\n\
\ hold her in her close chair within two yards of the stage, when she was\n\
\ able to take a full view of the whole performance.  It was my good\n\
\ fortune, that no ill accident happened in these entertainments; only once\n\
\ a fiery horse, that belonged to one of the captains, pawing with his\n\
\ hoof, struck a hole in my handkerchief, and his foot slipping, he\n\
\ overthrew his rider and himself; but I immediately relieved them both,\n\
\ and covering the hole with one hand, I set down the troop with the other,\n\
\ in the same manner as I took them up. The horse that fell was strained in\n\
\ the left shoulder, but the rider got no hurt; and I repaired my\n\
\ handkerchief as well as I could: however, I would not trust to the\n\
\ strength of it any more, in such dangerous enterprises.\n\
\ \n\
\ About two or three days before I was set at liberty, as I was\n\
\ entertaining the court with this kind of feat, there arrived an express\n\
\ to inform his majesty, that some of his subjects, riding near the place\n\
\ where I was first taken up, had seen a great black substance lying on the\n\
\ around, very oddly shaped, extending its edges round, as wide as his\n\
\ majesty's bedchamber, and rising up in the middle as high as a man; that\n\
\ it was no living creature, as they at first apprehended, for it lay on\n\
\ the grass without motion; and some of them had walked round it several\n\
\ times; that, by mounting upon each other's shoulders, they had got to the\n\
\ top, which was flat and even, and, stamping upon it, they found that it\n\
\ was hollow within; that they humbly conceived it might be something\n\
\ belonging to the man-mountain; and if his majesty pleased, they would\n\
\ undertake to bring it with only five horses.  I presently knew what they\n\
\ meant, and was glad at heart to receive this intelligence.  It seems,\n\
\ upon my first reaching the shore after our shipwreck, I was in such\n\
\ confusion, that before I came to the place where I went to sleep, my hat,\n\
\ which I had fastened with a string to my head while I was rowing, and had\n\
\ stuck on all the time I was swimming, fell off after I came to land; the\n\
\ string, as I conjecture, breaking by some accident, which I never\n\
\ observed, but thought my hat had been lost at sea.  I entreated his\n\
\ imperial majesty to give orders it might be brought to me as soon as\n\
\ possible, describing to him the use and the nature of it: and the next\n\
\ day the waggoners arrived with it, but not in a very good condition; they\n\
\ had bored two holes in the brim, within an inch and half of the edge, and\n\
\ fastened two hooks in the holes; these hooks were tied by a long cord to\n\
\ the harness, and thus my hat was dragged along for above half an English\n\
\ mile; but, the ground in that country being extremely smooth and level,\n\
\ it received less damage than I expected.\n\
\ \n\
\ Two days after this adventure, the emperor, having ordered that part of\n\
\ his army which quarters in and about his metropolis, to be in readiness,\n\
\ took a fancy of diverting himself in a very singular manner.  He desired\n\
\ I would stand like a Colossus, with my legs as far asunder as I\n\
\ conveniently could.  He then commanded his general (who was an old\n\
\ experienced leader, and a great patron of mine) to draw up the troops in\n\
\ close order, and march them under me; the foot by twenty-four abreast,\n\
\ and the horse by sixteen, with drums beating, colours flying, and pikes\n\
\ advanced.  This body consisted of three thousand foot, and a thousand\n\
\ horse.  His majesty gave orders, upon pain of death, that every soldier\n\
\ in his march should observe the strictest decency with regard to my\n\
\ person; which however could not prevent some of the younger officers from\n\
\ turning up their eyes as they passed under me: and, to confess the truth,\n\
\ my breeches were at that time in so ill a condition, that they afforded\n\
\ some opportunities for laughter and admiration.\n\
\ \n\
\ I had sent so many memorials and petitions for my liberty, that his\n\
\ majesty at length mentioned the matter, first in the cabinet, and then in\n\
\ a full council; where it was opposed by none, except Skyresh Bolgolam,\n\
\ who was pleased, without any provocation, to be my mortal enemy.  But it\n\
\ was carried against him by the whole board, and confirmed by the emperor.\n\
\ That minister was _galbet_, or admiral of the realm, very much in his\n\
\ master's confidence, and a person well versed in affairs, but of a morose\n\
\ and sour complexion.  However, he was at length persuaded to comply; but\n\
\ prevailed that the articles and conditions upon which I should be set\n\
\ free, and to which I must swear, should be drawn up by himself.  These\n\
\ articles were brought to me by Skyresh Bolgolam in person attended by two\n\
\ under-secretaries, and several persons of distinction.  After they were\n\
\ read, I was demanded to swear to the performance of them; first in the\n\
\ manner of my own country, and afterwards in the method prescribed by\n\
\ their laws; which was, to hold my right foot in my left hand, and to\n\
\ place the middle finger of my right hand on the crown of my head, and my\n\
\ thumb on the tip of my right ear.  But because the reader may be curious\n\
\ to have some idea of the style and manner of expression peculiar to that\n\
\ people, as well as to know the article upon which I recovered my liberty,\n\
\ I have made a translation of the whole instrument, word for word, as near\n\
\ as I was able, which I here offer to the public.\n\
\ \n\
\ \"Golbasto Momarem Evlame Gurdilo Shefin Mully Ully Gue, most mighty\n\
\ Emperor of Lilliput, delight and terror of the universe, whose dominions\n\
\ extend five thousand _blustrugs_ (about twelve miles in circumference) to\n\
\ the extremities of the globe; monarch of all monarchs, taller than the\n\
\ sons of men; whose feet press down to the centre, and whose head strikes\n\
\ against the sun; at whose nod the princes of the earth shake their knees;\n\
\ pleasant as the spring, comfortable as the summer, fruitful as autumn,\n\
\ dreadful as winter: his most sublime majesty proposes to the\n\
\ man-mountain, lately arrived at our celestial dominions, the following\n\
\ articles, which, by a solemn oath, he shall be obliged to perform:--\n\
\ \n\
\ \"1st, The man-mountain shall not depart from our dominions, without our\n\
\ license under our great seal.\n\
\ \n\
\ \"2d, He shall not presume to come into our metropolis, without our\n\
\ express order; at which time, the inhabitants shall have two hours\n\
\ warning to keep within doors.\n\
\ \n\
\ \"3d, The said man-mountain shall confine his walks to our principal high\n\
\ roads, and not offer to walk, or lie down, in a meadow or field of corn.\n\
\ \n\
\ \"4th, As he walks the said roads, he shall take the utmost care not to\n\
\ trample upon the bodies of any of our loving subjects, their horses, or\n\
\ carriages, nor take any of our subjects into his hands without their own\n\
\ consent.\n\
\ \n\
\ \"5th, If an express requires extraordinary despatch, the man-mountain\n\
\ shall be obliged to carry, in his pocket, the messenger and horse a six\n\
\ days journey, once in every moon, and return the said messenger back (if\n\
\ so required) safe to our imperial presence.\n\
\ \n\
\ \"6th, He shall be our ally against our enemies in the island of Blefuscu,\n\
\ and do his utmost to destroy their fleet, which is now preparing to\n\
\ invade us.\n\
\ \n\
\ \"7th, That the said man-mountain shall, at his times of leisure, be\n\
\ aiding and assisting to our workmen, in helping to raise certain great\n\
\ stones, towards covering the wall of the principal park, and other our\n\
\ royal buildings.\n\
\ \n\
\ \"8th, That the said man-mountain shall, in two moons' time, deliver in an\n\
\ exact survey of the circumference of our dominions, by a computation of\n\
\ his own paces round the coast.\n\
\ \n\
\ \"Lastly, That, upon his solemn oath to observe all the above articles,\n\
\ the said man-mountain shall have a daily allowance of meat and drink\n\
\ sufficient for the support of 1724 of our subjects, with free access to\n\
\ our royal person, and other marks of our favour.  Given at our palace at\n\
\ Belfaborac, the twelfth day of the ninety-first moon of our reign.\"\n\
\ \n\
\ I swore and subscribed to these articles with great cheerfulness and\n\
\ content, although some of them were not so honourable as I could have\n\
\ wished; which proceeded wholly from the malice of Skyresh Bolgolam, the\n\
\ high-admiral: whereupon my chains were immediately unlocked, and I was at\n\
\ full liberty.  The emperor himself, in person, did me the honour to be by\n\
\ at the whole ceremony.  I made my acknowledgements by prostrating myself\n\
\ at his majesty's feet: but he commanded me to rise; and after many\n\
\ gracious expressions, which, to avoid the censure of vanity, I shall not\n\
\ repeat, he added, \"that he hoped I should prove a useful servant, and\n\
\ well deserve all the favours he had already conferred upon me, or might\n\
\ do for the future.\"\n\
\ \n\
\ The reader may please to observe, that, in the last article of the\n\
\ recovery of my liberty, the emperor stipulates to allow me a quantity of\n\
\ meat and drink sufficient for the support of 1724 Lilliputians.  Some\n\
\ time after, asking a friend at court how they came to fix on that\n\
\ determinate number, he told me that his majesty's mathematicians, having\n\
\ taken the height of my body by the help of a quadrant, and finding it to\n\
\ exceed theirs in the proportion of twelve to one, they concluded from the\n\
\ similarity of their bodies, that mine must contain at least 1724 of\n\
\ theirs, and consequently would require as much food as was necessary to\n\
\ support that number of Lilliputians.  By which the reader may conceive an\n\
\ idea of the ingenuity of that people, as well as the prudent and exact\n\
\ economy of so great a prince.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER IV.\n\
\ \n\
\ \n\
\ Mildendo, the metropolis of Lilliput, described, together with the\n\
\ emperor's palace.  A conversation between the author and a principal\n\
\ secretary, concerning the affairs of that empire.  The author's offers to\n\
\ serve the emperor in his wars.\n\
\ \n\
\ The first request I made, after I had obtained my liberty, was, that I\n\
\ might have license to see Mildendo, the metropolis; which the emperor\n\
\ easily granted me, but with a special charge to do no hurt either to the\n\
\ inhabitants or their houses.  The people had notice, by proclamation, of\n\
\ my design to visit the town.  The wall which encompassed it is two feet\n\
\ and a half high, and at least eleven inches broad, so that a coach and\n\
\ horses may be driven very safely round it; and it is flanked with strong\n\
\ towers at ten feet distance.  I stepped over the great western gate, and\n\
\ passed very gently, and sidling, through the two principal streets, only\n\
\ in my short waistcoat, for fear of damaging the roofs and eaves of the\n\
\ houses with the skirts of my coat.  I walked with the utmost\n\
\ circumspection, to avoid treading on any stragglers who might remain in\n\
\ the streets, although the orders were very strict, that all people should\n\
\ keep in their houses, at their own peril.  The garret windows and tops of\n\
\ houses were so crowded with spectators, that I thought in all my travels\n\
\ I had not seen a more populous place.  The city is an exact square, each\n\
\ side of the wall being five hundred feet long.  The two great streets,\n\
\ which run across and divide it into four quarters, are five feet wide.\n\
\ The lanes and alleys, which I could not enter, but only view them as I\n\
\ passed, are from twelve to eighteen inches.  The town is capable of\n\
\ holding five hundred thousand souls: the houses are from three to five\n\
\ stories: the shops and markets well provided.\n\
\ \n\
\ The emperor's palace is in the centre of the city where the two great\n\
\ streets meet.  It is enclosed by a wall of two feet high, and twenty feet\n\
\ distance from the buildings.  I had his majesty's permission to step over\n\
\ this wall; and, the space being so wide between that and the palace, I\n\
\ could easily view it on every side.  The outward court is a square of\n\
\ forty feet, and includes two other courts: in the inmost are the royal\n\
\ apartments, which I was very desirous to see, but found it extremely\n\
\ difficult; for the great gates, from one square into another, were but\n\
\ eighteen inches high, and seven inches wide.  Now the buildings of the\n\
\ outer court were at least five feet high, and it was impossible for me to\n\
\ stride over them without infinite damage to the pile, though the walls\n\
\ were strongly built of hewn stone, and four inches thick.  At the same\n\
\ time the emperor had a great desire that I should see the magnificence of\n\
\ his palace; but this I was not able to do till three days after, which I\n\
\ spent in cutting down with my knife some of the largest trees in the\n\
\ royal park, about a hundred yards distant from the city.  Of these trees\n\
\ I made two stools, each about three feet high, and strong enough to bear\n\
\ my weight.  The people having received notice a second time, I went again\n\
\ through the city to the palace with my two stools in my hands.  When I\n\
\ came to the side of the outer court, I stood upon one stool, and took the\n\
\ other in my hand; this I lifted over the roof, and gently set it down on\n\
\ the space between the first and second court, which was eight feet wide.\n\
\ I then stept over the building very conveniently from one stool to the\n\
\ other, and drew up the first after me with a hooked stick.  By this\n\
\ contrivance I got into the inmost court; and, lying down upon my side, I\n\
\ applied my face to the windows of the middle stories, which were left\n\
\ open on purpose, and discovered the most splendid apartments that can be\n\
\ imagined.  There I saw the empress and the young princes, in their\n\
\ several lodgings, with their chief attendants about them.  Her imperial\n\
\ majesty was pleased to smile very graciously upon me, and gave me out of\n\
\ the window her hand to kiss.\n\
\ \n\
\ But I shall not anticipate the reader with further descriptions of this\n\
\ kind, because I reserve them for a greater work, which is now almost\n\
\ ready for the press; containing a general description of this empire,\n\
\ from its first erection, through along series of princes; with a\n\
\ particular account of their wars and politics, laws, learning, and\n\
\ religion; their plants and animals; their peculiar manners and customs,\n\
\ with other matters very curious and useful; my chief design at present\n\
\ being only to relate such events and transactions as happened to the\n\
\ public or to myself during a residence of about nine months in that\n\
\ empire.\n\
\ \n\
\ One morning, about a fortnight after I had obtained my liberty,\n\
\ Reldresal, principal secretary (as they style him) for private affairs,\n\
\ came to my house attended only by one servant.  He ordered his coach to\n\
\ wait at a distance, and desired I would give him an hours audience; which\n\
\ I readily consented to, on account of his quality and personal merits, as\n\
\ well as of the many good offices he had done me during my solicitations\n\
\ at court.  I offered to lie down that he might the more conveniently\n\
\ reach my ear, but he chose rather to let me hold him in my hand during\n\
\ our conversation.  He began with compliments on my liberty; said \"he\n\
\ might pretend to some merit in it;\" but, however, added, \"that if it had\n\
\ not been for the present situation of things at court, perhaps I might\n\
\ not have obtained it so soon.  For,\" said he, \"as flourishing a condition\n\
\ as we may appear to be in to foreigners, we labour under two mighty\n\
\ evils: a violent faction at home, and the danger of an invasion, by a\n\
\ most potent enemy, from abroad.  As to the first, you are to understand,\n\
\ that for about seventy moons past there have been two struggling parties\n\
\ in this empire, under the names of _Tramecksan_ and _Slamecksan_, from\n\
\ the high and low heels of their shoes, by which they distinguish\n\
\ themselves.  It is alleged, indeed, that the high heels are most\n\
\ agreeable to our ancient constitution; but, however this be, his majesty\n\
\ has determined to make use only of low heels in the administration of the\n\
\ government, and all offices in the gift of the crown, as you cannot but\n\
\ observe; and particularly that his majesty's imperial heels are lower at\n\
\ least by a _drurr_ than any of his court (_drurr_ is a measure about the\n\
\ fourteenth part of an inch).  The animosities between these two parties\n\
\ run so high, that they will neither eat, nor drink, nor talk with each\n\
\ other.  We compute the _Tramecksan_, or high heels, to exceed us in\n\
\ number; but the power is wholly on our side.  We apprehend his imperial\n\
\ highness, the heir to the crown, to have some tendency towards the high\n\
\ heels; at least we can plainly discover that one of his heels is higher\n\
\ than the other, which gives him a hobble in his gait.  Now, in the midst\n\
\ of these intestine disquiets, we are threatened with an invasion from the\n\
\ island of Blefuscu, which is the other great empire of the universe,\n\
\ almost as large and powerful as this of his majesty.  For as to what we\n\
\ have heard you affirm, that there are other kingdoms and states in the\n\
\ world inhabited by human creatures as large as yourself, our philosophers\n\
\ are in much doubt, and would rather conjecture that you dropped from the\n\
\ moon, or one of the stars; because it is certain, that a hundred mortals\n\
\ of your bulk would in a short time destroy all the fruits and cattle of\n\
\ his majesty's dominions: besides, our histories of six thousand moons\n\
\ make no mention of any other regions than the two great empires of\n\
\ Lilliput and Blefuscu.  Which two mighty powers have, as I was going to\n\
\ tell you, been engaged in a most obstinate war for six-and-thirty moons\n\
\ past.  It began upon the following occasion.  It is allowed on all hands,\n\
\ that the primitive way of breaking eggs, before we eat them, was upon the\n\
\ larger end; but his present majesty's grandfather, while he was a boy,\n\
\ going to eat an egg, and breaking it according to the ancient practice,\n\
\ happened to cut one of his fingers.  Whereupon the emperor his father\n\
\ published an edict, commanding all his subjects, upon great penalties, to\n\
\ break the smaller end of their eggs.  The people so highly resented this\n\
\ law, that our histories tell us, there have been six rebellions raised on\n\
\ that account; wherein one emperor lost his life, and another his crown.\n\
\ These civil commotions were constantly fomented by the monarchs of\n\
\ Blefuscu; and when they were quelled, the exiles always fled for refuge\n\
\ to that empire.  It is computed that eleven thousand persons have at\n\
\ several times suffered death, rather than submit to break their eggs at\n\
\ the smaller end.  Many hundred large volumes have been published upon\n\
\ this controversy: but the books of the Big-endians have been long\n\
\ forbidden, and the whole party rendered incapable by law of holding\n\
\ employments.  During the course of these troubles, the emperors of\n\
\ Blefusca did frequently expostulate by their ambassadors, accusing us of\n\
\ making a schism in religion, by offending against a fundamental doctrine\n\
\ of our great prophet Lustrog, in the fifty-fourth chapter of the\n\
\ Blundecral (which is their Alcoran).  This, however, is thought to be a\n\
\ mere strain upon the text; for the words are these: 'that all true\n\
\ believers break their eggs at the convenient end.'  And which is the\n\
\ convenient end, seems, in my humble opinion to be left to every man's\n\
\ conscience, or at least in the power of the chief magistrate to\n\
\ determine.  Now, the Big-endian exiles have found so much credit in the\n\
\ emperor of Blefuscu's court, and so much private assistance and\n\
\ encouragement from their party here at home, that a bloody war has been\n\
\ carried on between the two empires for six-and-thirty moons, with various\n\
\ success; during which time we have lost forty capital ships, and a much a\n\
\ greater number of smaller vessels, together with thirty thousand of our\n\
\ best seamen and soldiers; and the damage received by the enemy is\n\
\ reckoned to be somewhat greater than ours.  However, they have now\n\
\ equipped a numerous fleet, and are just preparing to make a descent upon\n\
\ us; and his imperial majesty, placing great confidence in your valour and\n\
\ strength, has commanded me to lay this account of his affairs before\n\
\ you.\"\n\
\ \n\
\ I desired the secretary to present my humble duty to the emperor; and to\n\
\ let him know, \"that I thought it would not become me, who was a\n\
\ foreigner, to interfere with parties; but I was ready, with the hazard of\n\
\ my life, to defend his person and state against all invaders.\"\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER V.\n\
\ \n\
\ \n\
\ The author, by an extraordinary stratagem, prevents an invasion.  A high\n\
\ title of honour is conferred upon him.  Ambassadors arrive from the\n\
\ emperor of Blefuscu, and sue for peace.  The empress's apartment on fire\n\
\ by an accident; the author instrumental in saving the rest of the palace.\n\
\ \n\
\ The empire of Blefuscu is an island situated to the north-east of\n\
\ Lilliput, from which it is parted only by a channel of eight hundred\n\
\ yards wide.  I had not yet seen it, and upon this notice of an intended\n\
\ invasion, I avoided appearing on that side of the coast, for fear of\n\
\ being discovered, by some of the enemy's ships, who had received no\n\
\ intelligence of me; all intercourse between the two empires having been\n\
\ strictly forbidden during the war, upon pain of death, and an embargo\n\
\ laid by our emperor upon all vessels whatsoever.  I communicated to his\n\
\ majesty a project I had formed of seizing the enemy's whole fleet; which,\n\
\ as our scouts assured us, lay at anchor in the harbour, ready to sail\n\
\ with the first fair wind.  I consulted the most experienced seamen upon\n\
\ the depth of the channel, which they had often plumbed; who told me, that\n\
\ in the middle, at high-water, it was seventy _glumgluffs_ deep, which is\n\
\ about six feet of European measure; and the rest of it fifty _glumgluffs_\n\
\ at most.  I walked towards the north-east coast, over against Blefuscu,\n\
\ where, lying down behind a hillock, I took out my small perspective\n\
\ glass, and viewed the enemy's fleet at anchor, consisting of about fifty\n\
\ men of war, and a great number of transports: I then came back to my\n\
\ house, and gave orders (for which I had a warrant) for a great quantity\n\
\ of the strongest cable and bars of iron.  The cable was about as thick as\n\
\ packthread and the bars of the length and size of a knitting-needle.  I\n\
\ trebled the cable to make it stronger, and for the same reason I twisted\n\
\ three of the iron bars together, bending the extremities into a hook.\n\
\ Having thus fixed fifty hooks to as many cables, I went back to the\n\
\ north-east coast, and putting off my coat, shoes, and stockings, walked\n\
\ into the sea, in my leathern jerkin, about half an hour before high\n\
\ water.  I waded with what haste I could, and swam in the middle about\n\
\ thirty yards, till I felt ground.  I arrived at the fleet in less than\n\
\ half an hour.  The enemy was so frightened when they saw me, that they\n\
\ leaped out of their ships, and swam to shore, where there could not be\n\
\ fewer than thirty thousand souls.  I then took my tackling, and,\n\
\ fastening a hook to the hole at the prow of each, I tied all the cords\n\
\ together at the end.  While I was thus employed, the enemy discharged\n\
\ several thousand arrows, many of which stuck in my hands and face, and,\n\
\ beside the excessive smart, gave me much disturbance in my work.  My\n\
\ greatest apprehension was for mine eyes, which I should have infallibly\n\
\ lost, if I had not suddenly thought of an expedient.  I kept, among other\n\
\ little necessaries, a pair of spectacles in a private pocket, which, as I\n\
\ observed before, had escaped the emperor's searchers.  These I took out\n\
\ and fastened as strongly as I could upon my nose, and thus armed, went on\n\
\ boldly with my work, in spite of the enemy's arrows, many of which struck\n\
\ against the glasses of my spectacles, but without any other effect,\n\
\ further than a little to discompose them.  I had now fastened all the\n\
\ hooks, and, taking the knot in my hand, began to pull; but not a ship\n\
\ would stir, for they were all too fast held by their anchors, so that the\n\
\ boldest part of my enterprise remained.  I therefore let go the cord, and\n\
\ leaving the looks fixed to the ships, I resolutely cut with my knife the\n\
\ cables that fastened the anchors, receiving about two hundred shots in my\n\
\ face and hands; then I took up the knotted end of the cables, to which my\n\
\ hooks were tied, and with great ease drew fifty of the enemy's largest\n\
\ men of war after me.\n\
\ \n\
\ The Blefuscudians, who had not the least imagination of what I intended,\n\
\ were at first confounded with astonishment.  They had seen me cut the\n\
\ cables, and thought my design was only to let the ships run adrift or\n\
\ fall foul on each other: but when they perceived the whole fleet moving\n\
\ in order, and saw me pulling at the end, they set up such a scream of\n\
\ grief and despair as it is almost impossible to describe or conceive.\n\
\ When I had got out of danger, I stopped awhile to pick out the arrows\n\
\ that stuck in my hands and face; and rubbed on some of the same ointment\n\
\ that was given me at my first arrival, as I have formerly mentioned.  I\n\
\ then took off my spectacles, and waiting about an hour, till the tide was\n\
\ a little fallen, I waded through the middle with my cargo, and arrived\n\
\ safe at the royal port of Lilliput.\n\
\ \n\
\ The emperor and his whole court stood on the shore, expecting the issue\n\
\ of this great adventure.  They saw the ships move forward in a large\n\
\ half-moon, but could not discern me, who was up to my breast in water.\n\
\ When I advanced to the middle of the channel, they were yet more in pain,\n\
\ because I was under water to my neck.  The emperor concluded me to be\n\
\ drowned, and that the enemy's fleet was approaching in a hostile manner:\n\
\ but he was soon eased of his fears; for the channel growing shallower\n\
\ every step I made, I came in a short time within hearing, and holding up\n\
\ the end of the cable, by which the fleet was fastened, I cried in a loud\n\
\ voice, \"Long live the most puissant king of Lilliput!\"  This great prince\n\
\ received me at my landing with all possible encomiums, and created me a\n\
\ _nardac_ upon the spot, which is the highest title of honour among them.\n\
\ \n\
\ His majesty desired I would take some other opportunity of bringing all\n\
\ the rest of his enemy's ships into his ports.  And so unmeasureable is\n\
\ the ambition of princes, that he seemed to think of nothing less than\n\
\ reducing the whole empire of Blefuscu into a province, and governing it,\n\
\ by a viceroy; of destroying the Big-endian exiles, and compelling that\n\
\ people to break the smaller end of their eggs, by which he would remain\n\
\ the sole monarch of the whole world.  But I endeavoured to divert him\n\
\ from this design, by many arguments drawn from the topics of policy as\n\
\ well as justice; and I plainly protested, \"that I would never be an\n\
\ instrument of bringing a free and brave people into slavery.\"  And, when\n\
\ the matter was debated in council, the wisest part of the ministry were\n\
\ of my opinion.\n\
\ \n\
\ This open bold declaration of mine was so opposite to the schemes and\n\
\ politics of his imperial majesty, that he could never forgive me.  He\n\
\ mentioned it in a very artful manner at council, where I was told that\n\
\ some of the wisest appeared, at least by their silence, to be of my\n\
\ opinion; but others, who were my secret enemies, could not forbear some\n\
\ expressions which, by a side-wind, reflected on me.  And from this time\n\
\ began an intrigue between his majesty and a junto of ministers,\n\
\ maliciously bent against me, which broke out in less than two months, and\n\
\ had like to have ended in my utter destruction.  Of so little weight are\n\
\ the greatest services to princes, when put into the balance with a\n\
\ refusal to gratify their passions.\n\
\ \n\
\ About three weeks after this exploit, there arrived a solemn embassy from\n\
\ Blefuscu, with humble offers of a peace, which was soon concluded, upon\n\
\ conditions very advantageous to our emperor, wherewith I shall not\n\
\ trouble the reader.  There were six ambassadors, with a train of about\n\
\ five hundred persons, and their entry was very magnificent, suitable to\n\
\ the grandeur of their master, and the importance of their business.  When\n\
\ their treaty was finished, wherein I did them several good offices by the\n\
\ credit I now had, or at least appeared to have, at court, their\n\
\ excellencies, who were privately told how much I had been their friend,\n\
\ made me a visit in form.  They began with many compliments upon my valour\n\
\ and generosity, invited me to that kingdom in the emperor their master's\n\
\ name, and desired me to show them some proofs of my prodigious strength,\n\
\ of which they had heard so many wonders; wherein I readily obliged them,\n\
\ but shall not trouble the reader with the particulars.\n\
\ \n\
\ When I had for some time entertained their excellencies, to their\n\
\ infinite satisfaction and surprise, I desired they would do me the honour\n\
\ to present my most humble respects to the emperor their master, the\n\
\ renown of whose virtues had so justly filled the whole world with\n\
\ admiration, and whose royal person I resolved to attend, before I\n\
\ returned to my own country.  Accordingly, the next time I had the honour\n\
\ to see our emperor, I desired his general license to wait on the\n\
\ Blefuscudian monarch, which he was pleased to grant me, as I could\n\
\ perceive, in a very cold manner; but could not guess the reason, till I\n\
\ had a whisper from a certain person, \"that Flimnap and Bolgolam had\n\
\ represented my intercourse with those ambassadors as a mark of\n\
\ disaffection;\" from which I am sure my heart was wholly free.  And this\n\
\ was the first time I began to conceive some imperfect idea of courts and\n\
\ ministers.\n\
\ \n\
\ It is to be observed, that these ambassadors spoke to me, by an\n\
\ interpreter, the languages of both empires differing as much from each\n\
\ other as any two in Europe, and each nation priding itself upon the\n\
\ antiquity, beauty, and energy of their own tongue, with an avowed\n\
\ contempt for that of their neighbour; yet our emperor, standing upon the\n\
\ advantage he had got by the seizure of their fleet, obliged them to\n\
\ deliver their credentials, and make their speech, in the Lilliputian\n\
\ tongue.  And it must be confessed, that from the great intercourse of\n\
\ trade and commerce between both realms, from the continual reception of\n\
\ exiles which is mutual among them, and from the custom, in each empire,\n\
\ to send their young nobility and richer gentry to the other, in order to\n\
\ polish themselves by seeing the world, and understanding men and manners;\n\
\ there are few persons of distinction, or merchants, or seamen, who dwell\n\
\ in the maritime parts, but what can hold conversation in both tongues; as\n\
\ I found some weeks after, when I went to pay my respects to the emperor\n\
\ of Blefuscu, which, in the midst of great misfortunes, through the malice\n\
\ of my enemies, proved a very happy adventure to me, as I shall relate in\n\
\ its proper place.\n\
\ \n\
\ The reader may remember, that when I signed those articles upon which I\n\
\ recovered my liberty, there were some which I disliked, upon account of\n\
\ their being too servile; neither could anything but an extreme necessity\n\
\ have forced me to submit.  But being now a _nardac_ of the highest rank\n\
\ in that empire, such offices were looked upon as below my dignity, and\n\
\ the emperor (to do him justice), never once mentioned them to me.\n\
\ However, it was not long before I had an opportunity of doing his\n\
\ majesty, at least as I then thought, a most signal service.  I was\n\
\ alarmed at midnight with the cries of many hundred people at my door; by\n\
\ which, being suddenly awaked, I was in some kind of terror.  I heard the\n\
\ word _Burglum_ repeated incessantly: several of the emperor's court,\n\
\ making their way through the crowd, entreated me to come immediately to\n\
\ the palace, where her imperial majesty's apartment was on fire, by the\n\
\ carelessness of a maid of honour, who fell asleep while she was reading a\n\
\ romance.  I got up in an instant; and orders being given to clear the way\n\
\ before me, and it being likewise a moonshine night, I made a shift to get\n\
\ to the palace without trampling on any of the people.  I found they had\n\
\ already applied ladders to the walls of the apartment, and were well\n\
\ provided with buckets, but the water was at some distance.  These buckets\n\
\ were about the size of large thimbles, and the poor people supplied me\n\
\ with them as fast as they could: but the flame was so violent that they\n\
\ did little good.  I might easily have stifled it with my coat, which I\n\
\ unfortunately left behind me for haste, and came away only in my leathern\n\
\ jerkin.  The case seemed wholly desperate and deplorable; and this\n\
\ magnificent palace would have infallibly been burnt down to the ground,\n\
\ if, by a presence of mind unusual to me, I had not suddenly thought of an\n\
\ expedient.  I had, the evening before, drunk plentifully of a most\n\
\ delicious wine called _glimigrim_, (the Blefuscudians call it _flunec_,\n\
\ but ours is esteemed the better sort,) which is very diuretic.  By the\n\
\ luckiest chance in the world, I had not discharged myself of any part of\n\
\ it.  The heat I had contracted by coming very near the flames, and by\n\
\ labouring to quench them, made the wine begin to operate by urine; which\n\
\ I voided in such a quantity, and applied so well to the proper places,\n\
\ that in three minutes the fire was wholly extinguished, and the rest of\n\
\ that noble pile, which had cost so many ages in erecting, preserved from\n\
\ destruction.\n\
\ \n\
\ It was now day-light, and I returned to my house without waiting to\n\
\ congratulate with the emperor: because, although I had done a very\n\
\ eminent piece of service, yet I could not tell how his majesty might\n\
\ resent the manner by which I had performed it: for, by the fundamental\n\
\ laws of the realm, it is capital in any person, of what quality soever,\n\
\ to make water within the precincts of the palace.  But I was a little\n\
\ comforted by a message from his majesty, \"that he would give orders to\n\
\ the grand justiciary for passing my pardon in form:\" which, however, I\n\
\ could not obtain; and I was privately assured, \"that the empress,\n\
\ conceiving the greatest abhorrence of what I had done, removed to the\n\
\ most distant side of the court, firmly resolved that those buildings\n\
\ should never be repaired for her use: and, in the presence of her chief\n\
\ confidents could not forbear vowing revenge.\"\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER VI.\n\
\ \n\
\ \n\
\ Of the inhabitants of Lilliput; their learning, laws, and customs; the\n\
\ manner of educating their children.  The author's way of living in that\n\
\ country.  His vindication of a great lady.\n\
\ \n\
\ Although I intend to leave the description of this empire to a particular\n\
\ treatise, yet, in the mean time, I am content to gratify the curious\n\
\ reader with some general ideas.  As the common size of the natives is\n\
\ somewhat under six inches high, so there is an exact proportion in all\n\
\ other animals, as well as plants and trees: for instance, the tallest\n\
\ horses and oxen are between four and five inches in height, the sheep an\n\
\ inch and half, more or less: their geese about the bigness of a sparrow,\n\
\ and so the several gradations downwards till you come to the smallest,\n\
\ which to my sight, were almost invisible; but nature has adapted the eyes\n\
\ of the Lilliputians to all objects proper for their view: they see with\n\
\ great exactness, but at no great distance.  And, to show the sharpness of\n\
\ their sight towards objects that are near, I have been much pleased with\n\
\ observing a cook pulling a lark, which was not so large as a common fly;\n\
\ and a young girl threading an invisible needle with invisible silk.\n\
\ Their tallest trees are about seven feet high: I mean some of those in\n\
\ the great royal park, the tops whereof I could but just reach with my\n\
\ fist clenched.  The other vegetables are in the same proportion; but this\n\
\ I leave to the reader's imagination.\n\
\ \n\
\ I shall say but little at present of their learning, which, for many\n\
\ ages, has flourished in all its branches among them: but their manner of\n\
\ writing is very peculiar, being neither from the left to the right, like\n\
\ the Europeans, nor from the right to the left, like the Arabians, nor\n\
\ from up to down, like the Chinese, but aslant, from one corner of the\n\
\ paper to the other, like ladies in England.\n\
\ \n\
\ They bury their dead with their heads directly downward, because they\n\
\ hold an opinion, that in eleven thousand moons they are all to rise\n\
\ again; in which period the earth (which they conceive to be flat) will\n\
\ turn upside down, and by this means they shall, at their resurrection, be\n\
\ found ready standing on their feet.  The learned among them confess the\n\
\ absurdity of this doctrine; but the practice still continues, in\n\
\ compliance to the vulgar.\n\
\ \n\
\ There are some laws and customs in this empire very peculiar; and if they\n\
\ were not so directly contrary to those of my own dear country, I should\n\
\ be tempted to say a little in their justification.  It is only to be\n\
\ wished they were as well executed.  The first I shall mention, relates to\n\
\ informers.  All crimes against the state, are punished here with the\n\
\ utmost severity; but, if the person accused makes his innocence plainly\n\
\ to appear upon his trial, the accuser is immediately put to an\n\
\ ignominious death; and out of his goods or lands the innocent person is\n\
\ quadruply recompensed for the loss of his time, for the danger he\n\
\ underwent, for the hardship of his imprisonment, and for all the charges\n\
\ he has been at in making his defence; or, if that fund be deficient, it\n\
\ is largely supplied by the crown.  The emperor also confers on him some\n\
\ public mark of his favour, and proclamation is made of his innocence\n\
\ through the whole city.\n\
\ \n\
\ They look upon fraud as a greater crime than theft, and therefore seldom\n\
\ fail to punish it with death; for they allege, that care and vigilance,\n\
\ with a very common understanding, may preserve a man's goods from\n\
\ thieves, but honesty has no defence against superior cunning; and, since\n\
\ it is necessary that there should be a perpetual intercourse of buying\n\
\ and selling, and dealing upon credit, where fraud is permitted and\n\
\ connived at, or has no law to punish it, the honest dealer is always\n\
\ undone, and the knave gets the advantage.  I remember, when I was once\n\
\ interceding with the emperor for a criminal who had wronged his master of\n\
\ a great sum of money, which he had received by order and ran away with;\n\
\ and happening to tell his majesty, by way of extenuation, that it was\n\
\ only a breach of trust, the emperor thought it monstrous in me to offer\n\
\ as a defence the greatest aggravation of the crime; and truly I had\n\
\ little to say in return, farther than the common answer, that different\n\
\ nations had different customs; for, I confess, I was heartily ashamed.\n\
\ {330}\n\
\ \n\
\ Although we usually call reward and punishment the two hinges upon which\n\
\ all government turns, yet I could never observe this maxim to be put in\n\
\ practice by any nation except that of Lilliput.  Whoever can there bring\n\
\ sufficient proof, that he has strictly observed the laws of his country\n\
\ for seventy-three moons, has a claim to certain privileges, according to\n\
\ his quality or condition of life, with a proportionable sum of money out\n\
\ of a fund appropriated for that use: he likewise acquires the title of\n\
\ _snilpall_, or legal, which is added to his name, but does not descend to\n\
\ his posterity.  And these people thought it a prodigious defect of policy\n\
\ among us, when I told them that our laws were enforced only by penalties,\n\
\ without any mention of reward.  It is upon this account that the image of\n\
\ Justice, in their courts of judicature, is formed with six eyes, two\n\
\ before, as many behind, and on each side one, to signify circumspection;\n\
\ with a bag of gold open in her right hand, and a sword sheathed in her\n\
\ left, to show she is more disposed to reward than to punish.\n\
\ \n\
\ In choosing persons for all employments, they have more regard to good\n\
\ morals than to great abilities; for, since government is necessary to\n\
\ mankind, they believe, that the common size of human understanding is\n\
\ fitted to some station or other; and that Providence never intended to\n\
\ make the management of public affairs a mystery to be comprehended only\n\
\ by a few persons of sublime genius, of which there seldom are three born\n\
\ in an age: but they suppose truth, justice, temperance, and the like, to\n\
\ be in every man's power; the practice of which virtues, assisted by\n\
\ experience and a good intention, would qualify any man for the service of\n\
\ his country, except where a course of study is required.  But they\n\
\ thought the want of moral virtues was so far from being supplied by\n\
\ superior endowments of the mind, that employments could never be put into\n\
\ such dangerous hands as those of persons so qualified; and, at least,\n\
\ that the mistakes committed by ignorance, in a virtuous disposition,\n\
\ would never be of such fatal consequence to the public weal, as the\n\
\ practices of a man, whose inclinations led him to be corrupt, and who had\n\
\ great abilities to manage, to multiply, and defend his corruptions.\n\
\ \n\
\ In like manner, the disbelief of a Divine Providence renders a man\n\
\ incapable of holding any public station; for, since kings avow themselves\n\
\ to be the deputies of Providence, the Lilliputians think nothing can be\n\
\ more absurd than for a prince to employ such men as disown the authority\n\
\ under which he acts.\n\
\ \n\
\ In relating these and the following laws, I would only be understood to\n\
\ mean the original institutions, and not the most scandalous corruptions,\n\
\ into which these people are fallen by the degenerate nature of man.  For,\n\
\ as to that infamous practice of acquiring great employments by dancing on\n\
\ the ropes, or badges of favour and distinction by leaping over sticks and\n\
\ creeping under them, the reader is to observe, that they were first\n\
\ introduced by the grandfather of the emperor now reigning, and grew to\n\
\ the present height by the gradual increase of party and faction.\n\
\ \n\
\ Ingratitude is among them a capital crime, as we read it to have been in\n\
\ some other countries: for they reason thus; that whoever makes ill\n\
\ returns to his benefactor, must needs be a common enemy to the rest of\n\
\ mankind, from whom he has received no obligation, and therefore such a\n\
\ man is not fit to live.\n\
\ \n\
\ Their notions relating to the duties of parents and children differ\n\
\ extremely from ours.  For, since the conjunction of male and female is\n\
\ founded upon the great law of nature, in order to propagate and continue\n\
\ the species, the Lilliputians will needs have it, that men and women are\n\
\ joined together, like other animals, by the motives of concupiscence; and\n\
\ that their tenderness towards their young proceeds from the like natural\n\
\ principle: for which reason they will never allow that a child is under\n\
\ any obligation to his father for begetting him, or to his mother for\n\
\ bringing him into the world; which, considering the miseries of human\n\
\ life, was neither a benefit in itself, nor intended so by his parents,\n\
\ whose thoughts, in their love encounters, were otherwise employed.  Upon\n\
\ these, and the like reasonings, their opinion is, that parents are the\n\
\ last of all others to be trusted with the education of their own\n\
\ children; and therefore they have in every town public nurseries, where\n\
\ all parents, except cottagers and labourers, are obliged to send their\n\
\ infants of both sexes to be reared and educated, when they come to the\n\
\ age of twenty moons, at which time they are supposed to have some\n\
\ rudiments of docility.  These schools are of several kinds, suited to\n\
\ different qualities, and both sexes.  They have certain professors well\n\
\ skilled in preparing children for such a condition of life as befits the\n\
\ rank of their parents, and their own capacities, as well as inclinations.\n\
\ I shall first say something of the male nurseries, and then of the\n\
\ female.\n\
\ \n\
\ The nurseries for males of noble or eminent birth, are provided with\n\
\ grave and learned professors, and their several deputies.  The clothes\n\
\ and food of the children are plain and simple.  They are bred up in the\n\
\ principles of honour, justice, courage, modesty, clemency, religion, and\n\
\ love of their country; they are always employed in some business, except\n\
\ in the times of eating and sleeping, which are very short, and two hours\n\
\ for diversions consisting of bodily exercises.  They are dressed by men\n\
\ till four years of age, and then are obliged to dress themselves,\n\
\ although their quality be ever so great; and the women attendant, who are\n\
\ aged proportionably to ours at fifty, perform only the most menial\n\
\ offices.  They are never suffered to converse with servants, but go\n\
\ together in smaller or greater numbers to take their diversions, and\n\
\ always in the presence of a professor, or one of his deputies; whereby\n\
\ they avoid those early bad impressions of folly and vice, to which our\n\
\ children are subject.  Their parents are suffered to see them only twice\n\
\ a year; the visit is to last but an hour; they are allowed to kiss the\n\
\ child at meeting and parting; but a professor, who always stands by on\n\
\ those occasions, will not suffer them to whisper, or use any fondling\n\
\ expressions, or bring any presents of toys, sweetmeats, and the like.\n\
\ \n\
\ The pension from each family for the education and entertainment of a\n\
\ child, upon failure of due payment, is levied by the emperor's officers.\n\
\ \n\
\ The nurseries for children of ordinary gentlemen, merchants, traders, and\n\
\ handicrafts, are managed proportionably after the same manner; only those\n\
\ designed for trades are put out apprentices at eleven years old, whereas\n\
\ those of persons of quality continue in their exercises till fifteen,\n\
\ which answers to twenty-one with us: but the confinement is gradually\n\
\ lessened for the last three years.\n\
\ \n\
\ In the female nurseries, the young girls of quality are educated much\n\
\ like the males, only they are dressed by orderly servants of their own\n\
\ sex; but always in the presence of a professor or deputy, till they come\n\
\ to dress themselves, which is at five years old.  And if it be found that\n\
\ these nurses ever presume to entertain the girls with frightful or\n\
\ foolish stories, or the common follies practised by chambermaids among\n\
\ us, they are publicly whipped thrice about the city, imprisoned for a\n\
\ year, and banished for life to the most desolate part of the country.\n\
\ Thus the young ladies are as much ashamed of being cowards and fools as\n\
\ the men, and despise all personal ornaments, beyond decency and\n\
\ cleanliness: neither did I perceive any difference in their education\n\
\ made by their difference of sex, only that the exercises of the females\n\
\ were not altogether so robust; and that some rules were given them\n\
\ relating to domestic life, and a smaller compass of learning was enjoined\n\
\ them: for their maxim is, that among peoples of quality, a wife should be\n\
\ always a reasonable and agreeable companion, because she cannot always be\n\
\ young.  When the girls are twelve years old, which among them is the\n\
\ marriageable age, their parents or guardians take them home, with great\n\
\ expressions of gratitude to the professors, and seldom without tears of\n\
\ the young lady and her companions.\n\
\ \n\
\ In the nurseries of females of the meaner sort, the children are\n\
\ instructed in all kinds of works proper for their sex, and their several\n\
\ degrees: those intended for apprentices are dismissed at seven years old,\n\
\ the rest are kept to eleven.\n\
\ \n\
\ The meaner families who have children at these nurseries, are obliged,\n\
\ besides their annual pension, which is as low as possible, to return to\n\
\ the steward of the nursery a small monthly share of their gettings, to be\n\
\ a portion for the child; and therefore all parents are limited in their\n\
\ expenses by the law.  For the Lilliputians think nothing can be more\n\
\ unjust, than for people, in subservience to their own appetites, to bring\n\
\ children into the world, and leave the burthen of supporting them on the\n\
\ public.  As to persons of quality, they give security to appropriate a\n\
\ certain sum for each child, suitable to their condition; and these funds\n\
\ are always managed with good husbandry and the most exact justice.\n\
\ \n\
\ The cottagers and labourers keep their children at home, their business\n\
\ being only to till and cultivate the earth, and therefore their education\n\
\ is of little consequence to the public: but the old and diseased among\n\
\ them, are supported by hospitals; for begging is a trade unknown in this\n\
\ empire.\n\
\ \n\
\ And here it may, perhaps, divert the curious reader, to give some account\n\
\ of my domestics, and my manner of living in this country, during a\n\
\ residence of nine months, and thirteen days.  Having a head mechanically\n\
\ turned, and being likewise forced by necessity, I had made for myself a\n\
\ table and chair convenient enough, out of the largest trees in the royal\n\
\ park.  Two hundred sempstresses were employed to make me shirts, and\n\
\ linen for my bed and table, all of the strongest and coarsest kind they\n\
\ could get; which, however, they were forced to quilt together in several\n\
\ folds, for the thickest was some degrees finer than lawn.  Their linen is\n\
\ usually three inches wide, and three feet make a piece.  The sempstresses\n\
\ took my measure as I lay on the ground, one standing at my neck, and\n\
\ another at my mid-leg, with a strong cord extended, that each held by the\n\
\ end, while a third measured the length of the cord with a rule of an inch\n\
\ long.  Then they measured my right thumb, and desired no more; for by a\n\
\ mathematical computation, that twice round the thumb is once round the\n\
\ wrist, and so on to the neck and the waist, and by the help of my old\n\
\ shirt, which I displayed on the ground before them for a pattern, they\n\
\ fitted me exactly.  Three hundred tailors were employed in the same\n\
\ manner to make me clothes; but they had another contrivance for taking my\n\
\ measure.  I kneeled down, and they raised a ladder from the ground to my\n\
\ neck; upon this ladder one of them mounted, and let fall a plumb-line\n\
\ from my collar to the floor, which just answered the length of my coat:\n\
\ but my waist and arms I measured myself.  When my clothes were finished,\n\
\ which was done in my house (for the largest of theirs would not have been\n\
\ able to hold them), they looked like the patch-work made by the ladies in\n\
\ England, only that mine were all of a colour.\n\
\ \n\
\ I had three hundred cooks to dress my victuals, in little convenient huts\n\
\ built about my house, where they and their families lived, and prepared\n\
\ me two dishes a-piece.  I took up twenty waiters in my hand, and placed\n\
\ them on the table: a hundred more attended below on the ground, some with\n\
\ dishes of meat, and some with barrels of wine and other liquors slung on\n\
\ their shoulders; all which the waiters above drew up, as I wanted, in a\n\
\ very ingenious manner, by certain cords, as we draw the bucket up a well\n\
\ in Europe.  A dish of their meat was a good mouthful, and a barrel of\n\
\ their liquor a reasonable draught.  Their mutton yields to ours, but\n\
\ their beef is excellent.  I have had a sirloin so large, that I have been\n\
\ forced to make three bites of it; but this is rare.  My servants were\n\
\ astonished to see me eat it, bones and all, as in our country we do the\n\
\ leg of a lark.  Their geese and turkeys I usually ate at a mouthful, and\n\
\ I confess they far exceed ours.  Of their smaller fowl I could take up\n\
\ twenty or thirty at the end of my knife.\n\
\ \n\
\ One day his imperial majesty, being informed of my way of living, desired\n\
\ \"that himself and his royal consort, with the young princes of the blood\n\
\ of both sexes, might have the happiness,\" as he was pleased to call it,\n\
\ \"of dining with me.\"  They came accordingly, and I placed them in chairs\n\
\ of state, upon my table, just over against me, with their guards about\n\
\ them.  Flimnap, the lord high treasurer, attended there likewise with his\n\
\ white staff; and I observed he often looked on me with a sour\n\
\ countenance, which I would not seem to regard, but ate more than usual,\n\
\ in honour to my dear country, as well as to fill the court with\n\
\ admiration.  I have some private reasons to believe, that this visit from\n\
\ his majesty gave Flimnap an opportunity of doing me ill offices to his\n\
\ master.  That minister had always been my secret enemy, though he\n\
\ outwardly caressed me more than was usual to the moroseness of his\n\
\ nature.  He represented to the emperor \"the low condition of his\n\
\ treasury; that he was forced to take up money at a great discount; that\n\
\ exchequer bills would not circulate under nine per cent. below par; that\n\
\ I had cost his majesty above a million and a half of _sprugs_\" (their\n\
\ greatest gold coin, about the bigness of a spangle) \"and, upon the whole,\n\
\ that it would be advisable in the emperor to take the first fair occasion\n\
\ of dismissing me.\"\n\
\ \n\
\ I am here obliged to vindicate the reputation of an excellent lady, who\n\
\ was an innocent sufferer upon my account.  The treasurer took a fancy to\n\
\ be jealous of his wife, from the malice of some evil tongues, who\n\
\ informed him that her grace had taken a violent affection for my person;\n\
\ and the court scandal ran for some time, that she once came privately to\n\
\ my lodging.  This I solemnly declare to be a most infamous falsehood,\n\
\ without any grounds, further than that her grace was pleased to treat me\n\
\ with all innocent marks of freedom and friendship.  I own she came often\n\
\ to my house, but always publicly, nor ever without three more in the\n\
\ coach, who were usually her sister and young daughter, and some\n\
\ particular acquaintance; but this was common to many other ladies of the\n\
\ court.  And I still appeal to my servants round, whether they at any time\n\
\ saw a coach at my door, without knowing what persons were in it.  On\n\
\ those occasions, when a servant had given me notice, my custom was to go\n\
\ immediately to the door, and, after paying my respects, to take up the\n\
\ coach and two horses very carefully in my hands (for, if there were six\n\
\ horses, the postillion always unharnessed four,) and place them on a\n\
\ table, where I had fixed a movable rim quite round, of five inches high,\n\
\ to prevent accidents.  And I have often had four coaches and horses at\n\
\ once on my table, full of company, while I sat in my chair, leaning my\n\
\ face towards them; and when I was engaged with one set, the coachmen\n\
\ would gently drive the others round my table.  I have passed many an\n\
\ afternoon very agreeably in these conversations.  But I defy the\n\
\ treasurer, or his two informers (I will name them, and let them make the\n\
\ best of it) Clustril and Drunlo, to prove that any person ever came to me\n\
\ _incognito_, except the secretary Reldresal, who was sent by express\n\
\ command of his imperial majesty, as I have before related.  I should not\n\
\ have dwelt so long upon this particular, if it had not been a point\n\
\ wherein the reputation of a great lady is so nearly concerned, to say\n\
\ nothing of my own; though I then had the honour to be a _nardac_, which\n\
\ the treasurer himself is not; for all the world knows, that he is only a\n\
\ _glumglum_, a title inferior by one degree, as that of a marquis is to a\n\
\ duke in England; yet I allow he preceded me in right of his post.  These\n\
\ false informations, which I afterwards came to the knowledge of by an\n\
\ accident not proper to mention, made the treasurer show his lady for some\n\
\ time an ill countenance, and me a worse; and although he was at last\n\
\ undeceived and reconciled to her, yet I lost all credit with him, and\n\
\ found my interest decline very fast with the emperor himself, who was,\n\
\ indeed, too much governed by that favourite.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER VII.\n\
\ \n\
\ \n\
\ The author, being informed of a design to accuse him of high-treason,\n\
\ makes his escape to Blefuscu.  His reception there.\n\
\ \n\
\ Before I proceed to give an account of my leaving this kingdom, it may be\n\
\ proper to inform the reader of a private intrigue which had been for two\n\
\ months forming against me.\n\
\ \n\
\ I had been hitherto, all my life, a stranger to courts, for which I was\n\
\ unqualified by the meanness of my condition.  I had indeed heard and read\n\
\ enough of the dispositions of great princes and ministers, but never\n\
\ expected to have found such terrible effects of them, in so remote a\n\
\ country, governed, as I thought, by very different maxims from those in\n\
\ Europe.\n\
\ \n\
\ When I was just preparing to pay my attendance on the emperor of\n\
\ Blefuscu, a considerable person at court (to whom I had been very\n\
\ serviceable, at a time when he lay under the highest displeasure of his\n\
\ imperial majesty) came to my house very privately at night, in a close\n\
\ chair, and, without sending his name, desired admittance.  The chairmen\n\
\ were dismissed; I put the chair, with his lordship in it, into my\n\
\ coat-pocket: and, giving orders to a trusty servant, to say I was\n\
\ indisposed and gone to sleep, I fastened the door of my house, placed the\n\
\ chair on the table, according to my usual custom, and sat down by it.\n\
\ After the common salutations were over, observing his lordship's\n\
\ countenance full of concern, and inquiring into the reason, he desired \"I\n\
\ would hear him with patience, in a matter that highly concerned my honour\n\
\ and my life.\"  His speech was to the following effect, for I took notes\n\
\ of it as soon as he left me:--\n\
\ \n\
\ \"You are to know,\" said he, \"that several committees of council have been\n\
\ lately called, in the most private manner, on your account; and it is but\n\
\ two days since his majesty came to a full resolution.\n\
\ \n\
\ \"You are very sensible that Skyresh Bolgolam\" (_galbet_, or high-admiral)\n\
\ \"has been your mortal enemy, almost ever since your arrival.  His\n\
\ original reasons I know not; but his hatred is increased since your great\n\
\ success against Blefuscu, by which his glory as admiral is much obscured.\n\
\ This lord, in conjunction with Flimnap the high-treasurer, whose enmity\n\
\ against you is notorious on account of his lady, Limtoc the general,\n\
\ Lalcon the chamberlain, and Balmuff the grand justiciary, have prepared\n\
\ articles of impeachment against you, for treason and other capital\n\
\ crimes.\"\n\
\ \n\
\ This preface made me so impatient, being conscious of my own merits and\n\
\ innocence, that I was going to interrupt him; when he entreated me to be\n\
\ silent, and thus proceeded:--\n\
\ \n\
\ \"Out of gratitude for the favours you have done me, I procured\n\
\ information of the whole proceedings, and a copy of the articles; wherein\n\
\ I venture my head for your service.\n\
\ \n\
\     \"'_Articles of Impeachment against_ QUINBUS FLESTRIN, (_the\n\
\     Man-Mountain_.)\n\
\ \n\
\     ARTICLE I.\n\
\ \n\
\     \"'Whereas, by a statute made in the reign of his imperial majesty\n\
\     Calin Deffar Plune, it is enacted, that, whoever shall make water\n\
\     within the precincts of the royal palace, shall be liable to the\n\
\     pains and penalties of high-treason; notwithstanding, the said\n\
\     Quinbus Flestrin, in open breach of the said law, under colour of\n\
\     extinguishing the fire kindled in the apartment of his majesty's most\n\
\     dear imperial consort, did maliciously, traitorously, and devilishly,\n\
\     by discharge of his urine, put out the said fire kindled in the said\n\
\     apartment, lying and being within the precincts of the said royal\n\
\     palace, against the statute in that case provided, etc. against the\n\
\     duty, etc.\n\
\ \n\
\     ARTICLE II.\n\
\ \n\
\     \"'That the said Quinbus Flestrin, having brought the imperial fleet\n\
\     of Blefuscu into the royal port, and being afterwards commanded by\n\
\     his imperial majesty to seize all the other ships of the said empire\n\
\     of Blefuscu, and reduce that empire to a province, to be governed by\n\
\     a viceroy from hence, and to destroy and put to death, not only all\n\
\     the Big-endian exiles, but likewise all the people of that empire who\n\
\     would not immediately forsake the Big-endian heresy, he, the said\n\
\     Flestrin, like a false traitor against his most auspicious, serene,\n\
\     imperial majesty, did petition to be excused from the said service,\n\
\     upon pretence of unwillingness to force the consciences, or destroy\n\
\     the liberties and lives of an innocent people.\n\
\ \n\
\     ARTICLE III.\n\
\ \n\
\     \"'That, whereas certain ambassadors arrived from the Court of\n\
\     Blefuscu, to sue for peace in his majesty's court, he, the said\n\
\     Flestrin, did, like a false traitor, aid, abet, comfort, and divert,\n\
\     the said ambassadors, although he knew them to be servants to a\n\
\     prince who was lately an open enemy to his imperial majesty, and in\n\
\     an open war against his said majesty.\n\
\ \n\
\     ARTICLE IV.\n\
\ \n\
\     \"'That the said Quinbus Flestrin, contrary to the duty of a faithful\n\
\     subject, is now preparing to make a voyage to the court and empire of\n\
\     Blefuscu, for which he has received only verbal license from his\n\
\     imperial majesty; and, under colour of the said license, does falsely\n\
\     and traitorously intend to take the said voyage, and thereby to aid,\n\
\     comfort, and abet the emperor of Blefuscu, so lately an enemy, and in\n\
\     open war with his imperial majesty aforesaid.'\n\
\ \n\
\ \"There are some other articles; but these are the most important, of\n\
\ which I have read you an abstract.\n\
\ \n\
\ \"In the several debates upon this impeachment, it must be confessed that\n\
\ his majesty gave many marks of his great lenity; often urging the\n\
\ services you had done him, and endeavouring to extenuate your crimes.\n\
\ The treasurer and admiral insisted that you should be put to the most\n\
\ painful and ignominious death, by setting fire to your house at night,\n\
\ and the general was to attend with twenty thousand men, armed with\n\
\ poisoned arrows, to shoot you on the face and hands.  Some of your\n\
\ servants were to have private orders to strew a poisonous juice on your\n\
\ shirts and sheets, which would soon make you tear your own flesh, and die\n\
\ in the utmost torture.  The general came into the same opinion; so that\n\
\ for a long time there was a majority against you; but his majesty\n\
\ resolving, if possible, to spare your life, at last brought off the\n\
\ chamberlain.\n\
\ \n\
\ \"Upon this incident, Reldresal, principal secretary for private affairs,\n\
\ who always approved himself your true friend, was commanded by the\n\
\ emperor to deliver his opinion, which he accordingly did; and therein\n\
\ justified the good thoughts you have of him.  He allowed your crimes to\n\
\ be great, but that still there was room for mercy, the most commendable\n\
\ virtue in a prince, and for which his majesty was so justly celebrated.\n\
\ He said, the friendship between you and him was so well known to the\n\
\ world, that perhaps the most honourable board might think him partial;\n\
\ however, in obedience to the command he had received, he would freely\n\
\ offer his sentiments.  That if his majesty, in consideration of your\n\
\ services, and pursuant to his own merciful disposition, would please to\n\
\ spare your life, and only give orders to put out both your eyes, he\n\
\ humbly conceived, that by this expedient justice might in some measure be\n\
\ satisfied, and all the world would applaud the lenity of the emperor, as\n\
\ well as the fair and generous proceedings of those who have the honour to\n\
\ be his counsellors.  That the loss of your eyes would be no impediment to\n\
\ your bodily strength, by which you might still be useful to his majesty;\n\
\ that blindness is an addition to courage, by concealing dangers from us;\n\
\ that the fear you had for your eyes, was the greatest difficulty in\n\
\ bringing over the enemy's fleet, and it would be sufficient for you to\n\
\ see by the eyes of the ministers, since the greatest princes do no more.\n\
\ \n\
\ \"This proposal was received with the utmost disapprobation by the whole\n\
\ board.  Bolgolam, the admiral, could not preserve his temper, but, rising\n\
\ up in fury, said, he wondered how the secretary durst presume to give his\n\
\ opinion for preserving the life of a traitor; that the services you had\n\
\ performed were, by all true reasons of state, the great aggravation of\n\
\ your crimes; that you, who were able to extinguish the fire by discharge\n\
\ of urine in her majesty's apartment (which he mentioned with horror),\n\
\ might, at another time, raise an inundation by the same means, to drown\n\
\ the whole palace; and the same strength which enabled you to bring over\n\
\ the enemy's fleet, might serve, upon the first discontent, to carry it\n\
\ back; that he had good reasons to think you were a Big-endian in your\n\
\ heart; and, as treason begins in the heart, before it appears in\n\
\ overt-acts, so he accused you as a traitor on that account, and therefore\n\
\ insisted you should be put to death.\n\
\ \n\
\ \"The treasurer was of the same opinion: he showed to what straits his\n\
\ majesty's revenue was reduced, by the charge of maintaining you, which\n\
\ would soon grow insupportable; that the secretary's expedient of putting\n\
\ out your eyes, was so far from being a remedy against this evil, that it\n\
\ would probably increase it, as is manifest from the common practice of\n\
\ blinding some kind of fowls, after which they fed the faster, and grew\n\
\ sooner fat; that his sacred majesty and the council, who are your judges,\n\
\ were, in their own consciences, fully convinced of your guilt, which was\n\
\ a sufficient argument to condemn you to death, without the formal proofs\n\
\ required by the strict letter of the law.\n\
\ \n\
\ \"But his imperial majesty, fully determined against capital punishment,\n\
\ was graciously pleased to say, that since the council thought the loss of\n\
\ your eyes too easy a censure, some other way may be inflicted hereafter.\n\
\ And your friend the secretary, humbly desiring to be heard again, in\n\
\ answer to what the treasurer had objected, concerning the great charge\n\
\ his majesty was at in maintaining you, said, that his excellency, who had\n\
\ the sole disposal of the emperor's revenue, might easily provide against\n\
\ that evil, by gradually lessening your establishment; by which, for want\n\
\ of sufficient for you would grow weak and faint, and lose your appetite,\n\
\ and consequently, decay, and consume in a few months; neither would the\n\
\ stench of your carcass be then so dangerous, when it should become more\n\
\ than half diminished; and immediately upon your death five or six\n\
\ thousand of his majesty's subjects might, in two or three days, cut your\n\
\ flesh from your bones, take it away by cart-loads, and bury it in distant\n\
\ parts, to prevent infection, leaving the skeleton as a monument of\n\
\ admiration to posterity.\n\
\ \n\
\ \"Thus, by the great friendship of the secretary, the whole affair was\n\
\ compromised.  It was strictly enjoined, that the project of starving you\n\
\ by degrees should be kept a secret; but the sentence of putting out your\n\
\ eyes was entered on the books; none dissenting, except Bolgolam the\n\
\ admiral, who, being a creature of the empress, was perpetually instigated\n\
\ by her majesty to insist upon your death, she having borne perpetual\n\
\ malice against you, on account of that infamous and illegal method you\n\
\ took to extinguish the fire in her apartment.\n\
\ \n\
\ \"In three days your friend the secretary will be directed to come to your\n\
\ house, and read before you the articles of impeachment; and then to\n\
\ signify the great lenity and favour of his majesty and council, whereby\n\
\ you are only condemned to the loss of your eyes, which his majesty does\n\
\ not question you will gratefully and humbly submit to; and twenty of his\n\
\ majesty's surgeons will attend, in order to see the operation well\n\
\ performed, by discharging very sharp-pointed arrows into the balls of\n\
\ your eyes, as you lie on the ground.\n\
\ \n\
\ \"I leave to your prudence what measures you will take; and to avoid\n\
\ suspicion, I must immediately return in as private a manner as I came.\"\n\
\ \n\
\ His lordship did so; and I remained alone, under many doubts and\n\
\ perplexities of mind.\n\
\ \n\
\ It was a custom introduced by this prince and his ministry (very\n\
\ different, as I have been assured, from the practice of former times,)\n\
\ that after the court had decreed any cruel execution, either to gratify\n\
\ the monarch's resentment, or the malice of a favourite, the emperor\n\
\ always made a speech to his whole council, expressing his great lenity\n\
\ and tenderness, as qualities known and confessed by all the world.  This\n\
\ speech was immediately published throughout the kingdom; nor did any\n\
\ thing terrify the people so much as those encomiums on his majesty's\n\
\ mercy; because it was observed, that the more these praises were enlarged\n\
\ and insisted on, the more inhuman was the punishment, and the sufferer\n\
\ more innocent.  Yet, as to myself, I must confess, having never been\n\
\ designed for a courtier, either by my birth or education, I was so ill a\n\
\ judge of things, that I could not discover the lenity and favour of this\n\
\ sentence, but conceived it (perhaps erroneously) rather to be rigorous\n\
\ than gentle.  I sometimes thought of standing my trial, for, although I\n\
\ could not deny the facts alleged in the several articles, yet I hoped\n\
\ they would admit of some extenuation.  But having in my life perused many\n\
\ state-trials, which I ever observed to terminate as the judges thought\n\
\ fit to direct, I durst not rely on so dangerous a decision, in so\n\
\ critical a juncture, and against such powerful enemies.  Once I was\n\
\ strongly bent upon resistance, for, while I had liberty the whole\n\
\ strength of that empire could hardly subdue me, and I might easily with\n\
\ stones pelt the metropolis to pieces; but I soon rejected that project\n\
\ with horror, by remembering the oath I had made to the emperor, the\n\
\ favours I received from him, and the high title of _nardac_ he conferred\n\
\ upon me.  Neither had I so soon learned the gratitude of courtiers, to\n\
\ persuade myself, that his majesty's present seventies acquitted me of all\n\
\ past obligations.\n\
\ \n\
\ At last, I fixed upon a resolution, for which it is probable I may incur\n\
\ some censure, and not unjustly; for I confess I owe the preserving of\n\
\ mine eyes, and consequently my liberty, to my own great rashness and want\n\
\ of experience; because, if I had then known the nature of princes and\n\
\ ministers, which I have since observed in many other courts, and their\n\
\ methods of treating criminals less obnoxious than myself, I should, with\n\
\ great alacrity and readiness, have submitted to so easy a punishment.\n\
\ But hurried on by the precipitancy of youth, and having his imperial\n\
\ majesty's license to pay my attendance upon the emperor of Blefuscu, I\n\
\ took this opportunity, before the three days were elapsed, to send a\n\
\ letter to my friend the secretary, signifying my resolution of setting\n\
\ out that morning for Blefuscu, pursuant to the leave I had got; and,\n\
\ without waiting for an answer, I went to that side of the island where\n\
\ our fleet lay.  I seized a large man of war, tied a cable to the prow,\n\
\ and, lifting up the anchors, I stripped myself, put my clothes (together\n\
\ with my coverlet, which I carried under my arm) into the vessel, and,\n\
\ drawing it after me, between wading and swimming arrived at the royal\n\
\ port of Blefuscu, where the people had long expected me: they lent me two\n\
\ guides to direct me to the capital city, which is of the same name.  I\n\
\ held them in my hands, till I came within two hundred yards of the gate,\n\
\ and desired them \"to signify my arrival to one of the secretaries, and\n\
\ let him know, I there waited his majesty's command.\"  I had an answer in\n\
\ about an hour, \"that his majesty, attended by the royal family, and great\n\
\ officers of the court, was coming out to receive me.\"  I advanced a\n\
\ hundred yards.  The emperor and his train alighted from their horses, the\n\
\ empress and ladies from their coaches, and I did not perceive they were\n\
\ in any fright or concern.  I lay on the ground to kiss his majesty's and\n\
\ the empress's hands.  I told his majesty, \"that I was come according to\n\
\ my promise, and with the license of the emperor my master, to have the\n\
\ honour of seeing so mighty a monarch, and to offer him any service in my\n\
\ power, consistent with my duty to my own prince;\" not mentioning a word\n\
\ of my disgrace, because I had hitherto no regular information of it, and\n\
\ might suppose myself wholly ignorant of any such design; neither could I\n\
\ reasonably conceive that the emperor would discover the secret, while I\n\
\ was out of his power; wherein, however, it soon appeared I was deceived.\n\
\ \n\
\ I shall not trouble the reader with the particular account of my\n\
\ reception at this court, which was suitable to the generosity of so great\n\
\ a prince; nor of the difficulties I was in for want of a house and bed,\n\
\ being forced to lie on the ground, wrapped up in my coverlet.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER VIII.\n\
\ \n\
\ \n\
\ The author, by a lucky accident, finds means to leave Blefuscu; and,\n\
\ after some difficulties, returns safe to his native country.\n\
\ \n\
\ Three days after my arrival, walking out of curiosity to the north-east\n\
\ coast of the island, I observed, about half a league off in the sea,\n\
\ somewhat that looked like a boat overturned.  I pulled off my shoes and\n\
\ stockings, and, wailing two or three hundred yards, I found the object to\n\
\ approach nearer by force of the tide; and then plainly saw it to be a\n\
\ real boat, which I supposed might by some tempest have been driven from a\n\
\ ship.  Whereupon, I returned immediately towards the city, and desired\n\
\ his imperial majesty to lend me twenty of the tallest vessels he had\n\
\ left, after the loss of his fleet, and three thousand seamen, under the\n\
\ command of his vice-admiral.  This fleet sailed round, while I went back\n\
\ the shortest way to the coast, where I first discovered the boat.  I\n\
\ found the tide had driven it still nearer.  The seamen were all provided\n\
\ with cordage, which I had beforehand twisted to a sufficient strength.\n\
\ When the ships came up, I stripped myself, and waded till I came within a\n\
\ hundred yards off the boat, after which I was forced to swim till I got\n\
\ up to it.  The seamen threw me the end of the cord, which I fastened to a\n\
\ hole in the fore-part of the boat, and the other end to a man of war; but\n\
\ I found all my labour to little purpose; for, being out of my depth, I\n\
\ was not able to work.  In this necessity I was forced to swim behind, and\n\
\ push the boat forward, as often as I could, with one of my hands; and the\n\
\ tide favouring me, I advanced so far that I could just hold up my chin\n\
\ and feel the ground.  I rested two or three minutes, and then gave the\n\
\ boat another shove, and so on, till the sea was no higher than my\n\
\ arm-pits; and now, the most laborious part being over, I took out my\n\
\ other cables, which were stowed in one of the ships, and fastened them\n\
\ first to the boat, and then to nine of the vessels which attended me; the\n\
\ wind being favourable, the seamen towed, and I shoved, until we arrived\n\
\ within forty yards of the shore; and, waiting till the tide was out, I\n\
\ got dry to the boat, and by the assistance of two thousand men, with\n\
\ ropes and engines, I made a shift to turn it on its bottom, and found it\n\
\ was but little damaged.\n\
\ \n\
\ I shall not trouble the reader with the difficulties I was under, by the\n\
\ help of certain paddles, which cost me ten days making, to get my boat to\n\
\ the royal port of Blefuscu, where a mighty concourse of people appeared\n\
\ upon my arrival, full of wonder at the sight of so prodigious a vessel.\n\
\ I told the emperor \"that my good fortune had thrown this boat in my way,\n\
\ to carry me to some place whence I might return into my native country;\n\
\ and begged his majesty's orders for getting materials to fit it up,\n\
\ together with his license to depart;\" which, after some kind\n\
\ expostulations, he was pleased to grant.\n\
\ \n\
\ I did very much wonder, in all this time, not to have heard of any\n\
\ express relating to me from our emperor to the court of Blefuscu.  But I\n\
\ was afterward given privately to understand, that his imperial majesty,\n\
\ never imagining I had the least notice of his designs, believed I was\n\
\ only gone to Blefuscu in performance of my promise, according to the\n\
\ license he had given me, which was well known at our court, and would\n\
\ return in a few days, when the ceremony was ended.  But he was at last in\n\
\ pain at my long absence; and after consulting with the treasurer and the\n\
\ rest of that cabal, a person of quality was dispatched with the copy of\n\
\ the articles against me.  This envoy had instructions to represent to the\n\
\ monarch of Blefuscu, \"the great lenity of his master, who was content to\n\
\ punish me no farther than with the loss of mine eyes; that I had fled\n\
\ from justice; and if I did not return in two hours, I should be deprived\n\
\ of my title of _nardac_, and declared a traitor.\"  The envoy further\n\
\ added, \"that in order to maintain the peace and amity between both\n\
\ empires, his master expected that his brother of Blefuscu would give\n\
\ orders to have me sent back to Lilliput, bound hand and foot, to be\n\
\ punished as a traitor.\"\n\
\ \n\
\ The emperor of Blefuscu, having taken three days to consult, returned an\n\
\ answer consisting of many civilities and excuses.  He said, \"that as for\n\
\ sending me bound, his brother knew it was impossible; that, although I\n\
\ had deprived him of his fleet, yet he owed great obligations to me for\n\
\ many good offices I had done him in making the peace.  That, however,\n\
\ both their majesties would soon be made easy; for I had found a\n\
\ prodigious vessel on the shore, able to carry me on the sea, which he had\n\
\ given orders to fit up, with my own assistance and direction; and he\n\
\ hoped, in a few weeks, both empires would be freed from so insupportable\n\
\ an encumbrance.\"\n\
\ \n\
\ With this answer the envoy returned to Lilliput; and the monarch of\n\
\ Blefuscu related to me all that had passed; offering me at the same time\n\
\ (but under the strictest confidence) his gracious protection, if I would\n\
\ continue in his service; wherein, although I believed him sincere, yet I\n\
\ resolved never more to put any confidence in princes or ministers, where\n\
\ I could possibly avoid it; and therefore, with all due acknowledgments\n\
\ for his favourable intentions, I humbly begged to be excused.  I told\n\
\ him, \"that since fortune, whether good or evil, had thrown a vessel in my\n\
\ way, I was resolved to venture myself on the ocean, rather than be an\n\
\ occasion of difference between two such mighty monarchs.\"  Neither did I\n\
\ find the emperor at all displeased; and I discovered, by a certain\n\
\ accident, that he was very glad of my resolution, and so were most of his\n\
\ ministers.\n\
\ \n\
\ These considerations moved me to hasten my departure somewhat sooner than\n\
\ I intended; to which the court, impatient to have me gone, very readily\n\
\ contributed.  Five hundred workmen were employed to make two sails to my\n\
\ boat, according to my directions, by quilting thirteen folds of their\n\
\ strongest linen together.  I was at the pains of making ropes and cables,\n\
\ by twisting ten, twenty, or thirty of the thickest and strongest of\n\
\ theirs.  A great stone that I happened to find, after a long search, by\n\
\ the sea-shore, served me for an anchor.  I had the tallow of three\n\
\ hundred cows, for greasing my boat, and other uses.  I was at incredible\n\
\ pains in cutting down some of the largest timber-trees, for oars and\n\
\ masts, wherein I was, however, much assisted by his majesty's\n\
\ ship-carpenters, who helped me in smoothing them, after I had done the\n\
\ rough work.\n\
\ \n\
\ In about a month, when all was prepared, I sent to receive his majesty's\n\
\ commands, and to take my leave.  The emperor and royal family came out of\n\
\ the palace; I lay down on my face to kiss his hand, which he very\n\
\ graciously gave me: so did the empress and young princes of the blood.\n\
\ His majesty presented me with fifty purses of two hundred _sprugs_\n\
\ a-piece, together with his picture at full length, which I put\n\
\ immediately into one of my gloves, to keep it from being hurt.  The\n\
\ ceremonies at my departure were too many to trouble the reader with at\n\
\ this time.\n\
\ \n\
\ I stored the boat with the carcases of a hundred oxen, and three hundred\n\
\ sheep, with bread and drink proportionable, and as much meat ready\n\
\ dressed as four hundred cooks could provide.  I took with me six cows and\n\
\ two bulls alive, with as many ewes and rams, intending to carry them into\n\
\ my own country, and propagate the breed.  And to feed them on board, I\n\
\ had a good bundle of hay, and a bag of corn.  I would gladly have taken a\n\
\ dozen of the natives, but this was a thing the emperor would by no means\n\
\ permit; and, besides a diligent search into my pockets, his majesty\n\
\ engaged my honour \"not to carry away any of his subjects, although with\n\
\ their own consent and desire.\"\n\
\ \n\
\ Having thus prepared all things as well as I was able, I set sail on the\n\
\ twenty-fourth day of September 1701, at six in the morning; and when I\n\
\ had gone about four-leagues to the northward, the wind being at\n\
\ south-east, at six in the evening I descried a small island, about half a\n\
\ league to the north-west.  I advanced forward, and cast anchor on the\n\
\ lee-side of the island, which seemed to be uninhabited.  I then took some\n\
\ refreshment, and went to my rest.  I slept well, and as I conjectured at\n\
\ least six hours, for I found the day broke in two hours after I awaked.\n\
\ It was a clear night.  I ate my breakfast before the sun was up; and\n\
\ heaving anchor, the wind being favourable, I steered the same course that\n\
\ I had done the day before, wherein I was directed by my pocket compass.\n\
\ My intention was to reach, if possible, one of those islands which I had\n\
\ reason to believe lay to the north-east of Van Diemen's Land.  I\n\
\ discovered nothing all that day; but upon the next, about three in the\n\
\ afternoon, when I had by my computation made twenty-four leagues from\n\
\ Blefuscu, I descried a sail steering to the south-east; my course was due\n\
\ east.  I hailed her, but could get no answer; yet I found I gained upon\n\
\ her, for the wind slackened.  I made all the sail I could, and in half an\n\
\ hour she spied me, then hung out her ancient, and discharged a gun.  It\n\
\ is not easy to express the joy I was in, upon the unexpected hope of once\n\
\ more seeing my beloved country, and the dear pledges I left in it.  The\n\
\ ship slackened her sails, and I came up with her between five and six in\n\
\ the evening, September 26th; but my heart leaped within me to see her\n\
\ English colours.  I put my cows and sheep into my coat-pockets, and got\n\
\ on board with all my little cargo of provisions.  The vessel was an\n\
\ English merchantman, returning from Japan by the North and South seas;\n\
\ the captain, Mr. John Biddel, of Deptford, a very civil man, and an\n\
\ excellent sailor.\n\
\ \n\
\ We were now in the latitude of 30 degrees south; there were about fifty\n\
\ men in the ship; and here I met an old comrade of mine, one Peter\n\
\ Williams, who gave me a good character to the captain.  This gentleman\n\
\ treated me with kindness, and desired I would let him know what place I\n\
\ came from last, and whither I was bound; which I did in a few words, but\n\
\ he thought I was raving, and that the dangers I underwent had disturbed\n\
\ my head; whereupon I took my black cattle and sheep out of my pocket,\n\
\ which, after great astonishment, clearly convinced him of my veracity.  I\n\
\ then showed him the gold given me by the emperor of Blefuscu, together\n\
\ with his majesty's picture at full length, and some other rarities of\n\
\ that country.  I gave him two purses of two hundreds _sprugs_ each, and\n\
\ promised, when we arrived in England, to make him a present of a cow and\n\
\ a sheep big with young.\n\
\ \n\
\ I shall not trouble the reader with a particular account of this voyage,\n\
\ which was very prosperous for the most part.  We arrived in the Downs on\n\
\ the 13th of April, 1702.  I had only one misfortune, that the rats on\n\
\ board carried away one of my sheep; I found her bones in a hole, picked\n\
\ clean from the flesh.  The rest of my cattle I got safe ashore, and set\n\
\ them a-grazing in a bowling-green at Greenwich, where the fineness of the\n\
\ grass made them feed very heartily, though I had always feared the\n\
\ contrary: neither could I possibly have preserved them in so long a\n\
\ voyage, if the captain had not allowed me some of his best biscuit,\n\
\ which, rubbed to powder, and mingled with water, was their constant food.\n\
\ The short time I continued in England, I made a considerable profit by\n\
\ showing my cattle to many persons of quality and others: and before I\n\
\ began my second voyage, I sold them for six hundred pounds.  Since my\n\
\ last return I find the breed is considerably increased, especially the\n\
\ sheep, which I hope will prove much to the advantage of the woollen\n\
\ manufacture, by the fineness of the fleeces.\n\
\ \n\
\ I stayed but two months with my wife and family, for my insatiable desire\n\
\ of seeing foreign countries, would suffer me to continue no longer.  I\n\
\ left fifteen hundred pounds with my wife, and fixed her in a good house\n\
\ at Redriff.  My remaining stock I carried with me, part in money and part\n\
\ in goods, in hopes to improve my fortunes.  My eldest uncle John had left\n\
\ me an estate in land, near Epping, of about thirty pounds a-year; and I\n\
\ had a long lease of the Black Bull in Fetter-Lane, which yielded me as\n\
\ much more; so that I was not in any danger of leaving my family upon the\n\
\ parish.  My son Johnny, named so after his uncle, was at the\n\
\ grammar-school, and a towardly child.  My daughter Betty (who is now well\n\
\ married, and has children) was then at her needle-work.  I took leave of\n\
\ my wife, and boy and girl, with tears on both sides, and went on board\n\
\ the Adventure, a merchant ship of three hundred tons, bound for Surat,\n\
\ captain John Nicholas, of Liverpool, commander.  But my account of this\n\
\ voyage must be referred to the Second Part of my Travels.\n\
\ \n\
\ \n\
\ \n\
\ \n\
\ \n\
\ \n\
\ PART II.  A VOYAGE TO BROBDINGNAG.\n\
\ \n\
\ \n\
\ \n\
\ \n\
\ \n\
\ \n\
\ CHAPTER I.\n\
\ \n\
\ \n\
\ A great storm described; the long boat sent to fetch water; the author\n\
\ goes with it to discover the country.  He is left on shore, is seized by\n\
\ one of the natives, and carried to a farmer's house.  His reception, with\n\
\ several accidents that happened there.  A description of the inhabitants.\n\
\ \n\
\ Having been condemned, by nature and fortune, to active and restless\n\
\ life, in two months after my return, I again left my native country, and\n\
\ took shipping in the Downs, on the 20th day of June, 1702, in the\n\
\ Adventure, Captain John Nicholas, a Cornish man, commander, bound for\n\
\ Surat.  We had a very prosperous gale, till we arrived at the Cape of\n\
\ Good Hope, where we landed for fresh water; but discovering a leak, we\n\
\ unshipped our goods and wintered there; for the captain falling sick of\n\
\ an ague, we could not leave the Cape till the end of March.  We then set\n\
\ sail, and had a good voyage till we passed the Straits of Madagascar; but\n\
\ having got northward of that island, and to about five degrees south\n\
\ latitude, the winds, which in those seas are observed to blow a constant\n\
\ equal gale between the north and west, from the beginning of December to\n\
\ the beginning of May, on the 19th of April began to blow with much\n\
\ greater violence, and more westerly than usual, continuing so for twenty\n\
\ days together: during which time, we were driven a little to the east of\n\
\ the Molucca Islands, and about three degrees northward of the line, as\n\
\ our captain found by an observation he took the 2nd of May, at which time\n\
\ the wind ceased, and it was a perfect calm, whereat I was not a little\n\
\ rejoiced.  But he, being a man well experienced in the navigation of\n\
\ those seas, bid us all prepare against a storm, which accordingly\n\
\ happened the day following: for the southern wind, called the southern\n\
\ monsoon, began to set in.\n\
\ \n\
\ Finding it was likely to overblow, we took in our sprit-sail, and stood\n\
\ by to hand the fore-sail; but making foul weather, we looked the guns\n\
\ were all fast, and handed the mizen.  The ship lay very broad off, so we\n\
\ thought it better spooning before the sea, than trying or hulling.  We\n\
\ reefed the fore-sail and set him, and hauled aft the fore-sheet; the helm\n\
\ was hard a-weather.  The ship wore bravely.  We belayed the fore\n\
\ down-haul; but the sail was split, and we hauled down the yard, and got\n\
\ the sail into the ship, and unbound all the things clear of it.  It was a\n\
\ very fierce storm; the sea broke strange and dangerous.  We hauled off\n\
\ upon the laniard of the whip-staff, and helped the man at the helm.  We\n\
\ would not get down our topmast, but let all stand, because she scudded\n\
\ before the sea very well, and we knew that the top-mast being aloft, the\n\
\ ship was the wholesomer, and made better way through the sea, seeing we\n\
\ had sea-room.  When the storm was over, we set fore-sail and main-sail,\n\
\ and brought the ship to.  Then we set the mizen, main-top-sail, and the\n\
\ fore-top-sail.  Our course was east-north-east, the wind was at\n\
\ south-west.  We got the starboard tacks aboard, we cast off our\n\
\ weather-braces and lifts; we set in the lee-braces, and hauled forward by\n\
\ the weather-bowlings, and hauled them tight, and belayed them, and hauled\n\
\ over the mizen tack to windward, and kept her full and by as near as she\n\
\ would lie.\n\
\ \n\
\ During this storm, which was followed by a strong wind west-south-west,\n\
\ we were carried, by my computation, about five hundred leagues to the\n\
\ east, so that the oldest sailor on board could not tell in what part of\n\
\ the world we were.  Our provisions held out well, our ship was staunch,\n\
\ and our crew all in good health; but we lay in the utmost distress for\n\
\ water.  We thought it best to hold on the same course, rather than turn\n\
\ more northerly, which might have brought us to the north-west part of\n\
\ Great Tartary, and into the Frozen Sea.\n\
\ \n\
\ On the 16th day of June, 1703, a boy on the top-mast discovered land.  On\n\
\ the 17th, we came in full view of a great island, or continent (for we\n\
\ knew not whether;) on the south side whereof was a small neck of land\n\
\ jutting out into the sea, and a creek too shallow to hold a ship of above\n\
\ one hundred tons.  We cast anchor within a league of this creek, and our\n\
\ captain sent a dozen of his men well armed in the long-boat, with vessels\n\
\ for water, if any could be found.  I desired his leave to go with them,\n\
\ that I might see the country, and make what discoveries I could.  When we\n\
\ came to land we saw no river or spring, nor any sign of inhabitants.  Our\n\
\ men therefore wandered on the shore to find out some fresh water near the\n\
\ sea, and I walked alone about a mile on the other side, where I observed\n\
\ the country all barren and rocky.  I now began to be weary, and seeing\n\
\ nothing to entertain my curiosity, I returned gently down towards the\n\
\ creek; and the sea being full in my view, I saw our men already got into\n\
\ the boat, and rowing for life to the ship.  I was going to holla after\n\
\ them, although it had been to little purpose, when I observed a huge\n\
\ creature walking after them in the sea, as fast as he could: he waded not\n\
\ much deeper than his knees, and took prodigious strides: but our men had\n\
\ the start of him half a league, and, the sea thereabouts being full of\n\
\ sharp-pointed rocks, the monster was not able to overtake the boat.  This\n\
\ I was afterwards told, for I durst not stay to see the issue of the\n\
\ adventure; but ran as fast as I could the way I first went, and then\n\
\ climbed up a steep hill, which gave me some prospect of the country.  I\n\
\ found it fully cultivated; but that which first surprised me was the\n\
\ length of the grass, which, in those grounds that seemed to be kept for\n\
\ hay, was about twenty feet high.\n\
\ \n\
\ I fell into a high road, for so I took it to be, though it served to the\n\
\ inhabitants only as a foot-path through a field of barley.  Here I walked\n\
\ on for some time, but could see little on either side, it being now near\n\
\ harvest, and the corn rising at least forty feet.  I was an hour walking\n\
\ to the end of this field, which was fenced in with a hedge of at least\n\
\ one hundred and twenty feet high, and the trees so lofty that I could\n\
\ make no computation of their altitude.  There was a stile to pass from\n\
\ this field into the next.  It had four steps, and a stone to cross over\n\
\ when you came to the uppermost.  It was impossible for me to climb this\n\
\ stile, because every step was six-feet high, and the upper stone about\n\
\ twenty.  I was endeavouring to find some gap in the hedge, when I\n\
\ discovered one of the inhabitants in the next field, advancing towards\n\
\ the stile, of the same size with him whom I saw in the sea pursuing our\n\
\ boat.  He appeared as tall as an ordinary spire steeple, and took about\n\
\ ten yards at every stride, as near as I could guess.  I was struck with\n\
\ the utmost fear and astonishment, and ran to hide myself in the corn,\n\
\ whence I saw him at the top of the stile looking back into the next field\n\
\ on the right hand, and heard him call in a voice many degrees louder than\n\
\ a speaking-trumpet: but the noise was so high in the air, that at first I\n\
\ certainly thought it was thunder.  Whereupon seven monsters, like\n\
\ himself, came towards him with reaping-hooks in their hands, each hook\n\
\ about the largeness of six scythes.  These people were not so well clad\n\
\ as the first, whose servants or labourers they seemed to be; for, upon\n\
\ some words he spoke, they went to reap the corn in the field where I lay.\n\
\ I kept from them at as great a distance as I could, but was forced to\n\
\ move with extreme difficulty, for the stalks of the corn were sometimes\n\
\ not above a foot distant, so that I could hardly squeeze my body betwixt\n\
\ them.  However, I made a shift to go forward, till I came to a part of\n\
\ the field where the corn had been laid by the rain and wind.  Here it was\n\
\ impossible for me to advance a step; for the stalks were so interwoven,\n\
\ that I could not creep through, and the beards of the fallen ears so\n\
\ strong and pointed, that they pierced through my clothes into my flesh.\n\
\ At the same time I heard the reapers not a hundred yards behind me.\n\
\ Being quite dispirited with toil, and wholly overcome by grief and\n\
\ dispair, I lay down between two ridges, and heartily wished I might there\n\
\ end my days.  I bemoaned my desolate widow and fatherless children.  I\n\
\ lamented my own folly and wilfulness, in attempting a second voyage,\n\
\ against the advice of all my friends and relations.  In this terrible\n\
\ agitation of mind, I could not forbear thinking of Lilliput, whose\n\
\ inhabitants looked upon me as the greatest prodigy that ever appeared in\n\
\ the world; where I was able to draw an imperial fleet in my hand, and\n\
\ perform those other actions, which will be recorded for ever in the\n\
\ chronicles of that empire, while posterity shall hardly believe them,\n\
\ although attested by millions.  I reflected what a mortification it must\n\
\ prove to me, to appear as inconsiderable in this nation, as one single\n\
\ Lilliputian would be among us.  But this I conceived was to be the least\n\
\ of my misfortunes; for, as human creatures are observed to be more savage\n\
\ and cruel in proportion to their bulk, what could I expect but to be a\n\
\ morsel in the mouth of the first among these enormous barbarians that\n\
\ should happen to seize me?  Undoubtedly philosophers are in the right,\n\
\ when they tell us that nothing is great or little otherwise than by\n\
\ comparison.  It might have pleased fortune, to have let the Lilliputians\n\
\ find some nation, where the people were as diminutive with respect to\n\
\ them, as they were to me.  And who knows but that even this prodigious\n\
\ race of mortals might be equally overmatched in some distant part of the\n\
\ world, whereof we have yet no discovery.\n\
\ \n\
\ Scared and confounded as I was, I could not forbear going on with these\n\
\ reflections, when one of the reapers, approaching within ten yards of the\n\
\ ridge where I lay, made me apprehend that with the next step I should be\n\
\ squashed to death under his foot, or cut in two with his reaping-hook.\n\
\ And therefore, when he was again about to move, I screamed as loud as\n\
\ fear could make me: whereupon the huge creature trod short, and, looking\n\
\ round about under him for some time, at last espied me as I lay on the\n\
\ ground.  He considered awhile, with the caution of one who endeavours to\n\
\ lay hold on a small dangerous animal in such a manner that it shall not\n\
\ be able either to scratch or bite him, as I myself have sometimes done\n\
\ with a weasel in England.  At length he ventured to take me behind, by\n\
\ the middle, between his fore-finger and thumb, and brought me within\n\
\ three yards of his eyes, that he might behold my shape more perfectly.  I\n\
\ guessed his meaning, and my good fortune gave me so much presence of\n\
\ mind, that I resolved not to struggle in the least as he held me in the\n\
\ air above sixty feet from the ground, although he grievously pinched my\n\
\ sides, for fear I should slip through his fingers.  All I ventured was to\n\
\ raise mine eyes towards the sun, and place my hands together in a\n\
\ supplicating posture, and to speak some words in a humble melancholy\n\
\ tone, suitable to the condition I then was in: for I apprehended every\n\
\ moment that he would dash me against the ground, as we usually do any\n\
\ little hateful animal, which we have a mind to destroy.  But my good star\n\
\ would have it, that he appeared pleased with my voice and gestures, and\n\
\ began to look upon me as a curiosity, much wondering to hear me pronounce\n\
\ articulate words, although he could not understand them.  In the mean\n\
\ time I was not able to forbear groaning and shedding tears, and turning\n\
\ my head towards my sides; letting him know, as well as I could, how\n\
\ cruelly I was hurt by the pressure of his thumb and finger.  He seemed to\n\
\ apprehend my meaning; for, lifting up the lappet of his coat, he put me\n\
\ gently into it, and immediately ran along with me to his master, who was\n\
\ a substantial farmer, and the same person I had first seen in the field.\n\
\ \n\
\ The farmer having (as I suppose by their talk) received such an account\n\
\ of me as his servant could give him, took a piece of a small straw, about\n\
\ the size of a walking-staff, and therewith lifted up the lappets of my\n\
\ coat; which it seems he thought to be some kind of covering that nature\n\
\ had given me.  He blew my hairs aside to take a better view of my face.\n\
\ He called his hinds about him, and asked them, as I afterwards learned,\n\
\ whether they had ever seen in the fields any little creature that\n\
\ resembled me.  He then placed me softly on the ground upon all fours, but\n\
\ I got immediately up, and walked slowly backward and forward, to let\n\
\ those people see I had no intent to run away.  They all sat down in a\n\
\ circle about me, the better to observe my motions.  I pulled off my hat,\n\
\ and made a low bow towards the farmer.  I fell on my knees, and lifted up\n\
\ my hands and eyes, and spoke several words as loud as I could: I took a\n\
\ purse of gold out of my pocket, and humbly presented it to him.  He\n\
\ received it on the palm of his hand, then applied it close to his eye to\n\
\ see what it was, and afterwards turned it several times with the point of\n\
\ a pin (which he took out of his sleeve,) but could make nothing of it.\n\
\ Whereupon I made a sign that he should place his hand on the ground.  I\n\
\ then took the purse, and, opening it, poured all the gold into his palm.\n\
\ There were six Spanish pieces of four pistoles each, beside twenty or\n\
\ thirty smaller coins.  I saw him wet the tip of his little finger upon\n\
\ his tongue, and take up one of my largest pieces, and then another; but\n\
\ he seemed to be wholly ignorant what they were.  He made me a sign to put\n\
\ them again into my purse, and the purse again into my pocket, which,\n\
\ after offering it to him several times, I thought it best to do.\n\
\ \n\
\ The farmer, by this time, was convinced I must be a rational creature.\n\
\ He spoke often to me; but the sound of his voice pierced my ears like\n\
\ that of a water-mill, yet his words were articulate enough.  I answered\n\
\ as loud as I could in several languages, and he often laid his ear within\n\
\ two yards of me: but all in vain, for we were wholly unintelligible to\n\
\ each other.  He then sent his servants to their work, and taking his\n\
\ handkerchief out of his pocket, he doubled and spread it on his left\n\
\ hand, which he placed flat on the ground with the palm upward, making me\n\
\ a sign to step into it, as I could easily do, for it was not above a foot\n\
\ in thickness.  I thought it my part to obey, and, for fear of falling,\n\
\ laid myself at full length upon the handkerchief, with the remainder of\n\
\ which he lapped me up to the head for further security, and in this\n\
\ manner carried me home to his house.  There he called his wife, and\n\
\ showed me to her; but she screamed and ran back, as women in England do\n\
\ at the sight of a toad or a spider.  However, when she had a while seen\n\
\ my behaviour, and how well I observed the signs her husband made, she was\n\
\ soon reconciled, and by degrees grew extremely tender of me.\n\
\ \n\
\ It was about twelve at noon, and a servant brought in dinner.  It was\n\
\ only one substantial dish of meat (fit for the plain condition of a\n\
\ husbandman,) in a dish of about four-and-twenty feet diameter.  The\n\
\ company were, the farmer and his wife, three children, and an old\n\
\ grandmother.  When they were sat down, the farmer placed me at some\n\
\ distance from him on the table, which was thirty feet high from the\n\
\ floor.  I was in a terrible fright, and kept as far as I could from the\n\
\ edge, for fear of falling.  The wife minced a bit of meat, then crumbled\n\
\ some bread on a trencher, and placed it before me.  I made her a low bow,\n\
\ took out my knife and fork, and fell to eat, which gave them exceeding\n\
\ delight.  The mistress sent her maid for a small dram cup, which held\n\
\ about two gallons, and filled it with drink; I took up the vessel with\n\
\ much difficulty in both hands, and in a most respectful manner drank to\n\
\ her ladyship's health, expressing the words as loud as I could in\n\
\ English, which made the company laugh so heartily, that I was almost\n\
\ deafened with the noise.  This liquor tasted like a small cider, and was\n\
\ not unpleasant.  Then the master made me a sign to come to his trencher\n\
\ side; but as I walked on the table, being in great surprise all the time,\n\
\ as the indulgent reader will easily conceive and excuse, I happened to\n\
\ stumble against a crust, and fell flat on my face, but received no hurt.\n\
\ I got up immediately, and observing the good people to be in much\n\
\ concern, I took my hat (which I held under my arm out of good manners,)\n\
\ and waving it over my head, made three huzzas, to show I had got no\n\
\ mischief by my fall.  But advancing forward towards my master (as I shall\n\
\ henceforth call him,) his youngest son, who sat next to him, an arch boy\n\
\ of about ten years old, took me up by the legs, and held me so high in\n\
\ the air, that I trembled every limb: but his father snatched me from him,\n\
\ and at the same time gave him such a box on the left ear, as would have\n\
\ felled an European troop of horse to the earth, ordering him to be taken\n\
\ from the table.  But being afraid the boy might owe me a spite, and well\n\
\ remembering how mischievous all children among us naturally are to\n\
\ sparrows, rabbits, young kittens, and puppy dogs, I fell on my knees, and\n\
\ pointing to the boy, made my master to understand, as well as I could,\n\
\ that I desired his son might be pardoned.  The father complied, and the\n\
\ lad took his seat again, whereupon I went to him, and kissed his hand,\n\
\ which my master took, and made him stroke me gently with it.\n\
\ \n\
\ In the midst of dinner, my mistress's favourite cat leaped into her lap.\n\
\ I heard a noise behind me like that of a dozen stocking-weavers at work;\n\
\ and turning my head, I found it proceeded from the purring of that\n\
\ animal, who seemed to be three times larger than an ox, as I computed by\n\
\ the view of her head, and one of her paws, while her mistress was feeding\n\
\ and stroking her.  The fierceness of this creature's countenance\n\
\ altogether discomposed me; though I stood at the farther end of the\n\
\ table, above fifty feet off; and although my mistress held her fast, for\n\
\ fear she might give a spring, and seize me in her talons.  But it\n\
\ happened there was no danger, for the cat took not the least notice of me\n\
\ when my master placed me within three yards of her.  And as I have been\n\
\ always told, and found true by experience in my travels, that flying or\n\
\ discovering fear before a fierce animal, is a certain way to make it\n\
\ pursue or attack you, so I resolved, in this dangerous juncture, to show\n\
\ no manner of concern.  I walked with intrepidity five or six times before\n\
\ the very head of the cat, and came within half a yard of her; whereupon\n\
\ she drew herself back, as if she were more afraid of me: I had less\n\
\ apprehension concerning the dogs, whereof three or four came into the\n\
\ room, as it is usual in farmers' houses; one of which was a mastiff,\n\
\ equal in bulk to four elephants, and another a greyhound, somewhat taller\n\
\ than the mastiff, but not so large.\n\
\ \n\
\ When dinner was almost done, the nurse came in with a child of a year old\n\
\ in her arms, who immediately spied me, and began a squall that you might\n\
\ have heard from London-Bridge to Chelsea, after the usual oratory of\n\
\ infants, to get me for a plaything.  The mother, out of pure indulgence,\n\
\ took me up, and put me towards the child, who presently seized me by the\n\
\ middle, and got my head into his mouth, where I roared so loud that the\n\
\ urchin was frighted, and let me drop, and I should infallibly have broke\n\
\ my neck, if the mother had not held her apron under me.  The nurse, to\n\
\ quiet her babe, made use of a rattle which was a kind of hollow vessel\n\
\ filled with great stones, and fastened by a cable to the child's waist:\n\
\ but all in vain; so that she was forced to apply the last remedy by\n\
\ giving it suck.  I must confess no object ever disgusted me so much as\n\
\ the sight of her monstrous breast, which I cannot tell what to compare\n\
\ with, so as to give the curious reader an idea of its bulk, shape, and\n\
\ colour.  It stood prominent six feet, and could not be less than sixteen\n\
\ in circumference.  The nipple was about half the bigness of my head, and\n\
\ the hue both of that and the dug, so varied with spots, pimples, and\n\
\ freckles, that nothing could appear more nauseous: for I had a near sight\n\
\ of her, she sitting down, the more conveniently to give suck, and I\n\
\ standing on the table.  This made me reflect upon the fair skins of our\n\
\ English ladies, who appear so beautiful to us, only because they are of\n\
\ our own size, and their defects not to be seen but through a magnifying\n\
\ glass; where we find by experiment that the smoothest and whitest skins\n\
\ look rough, and coarse, and ill-coloured.\n\
\ \n\
\ I remember when I was at Lilliput, the complexion of those diminutive\n\
\ people appeared to me the fairest in the world; and talking upon this\n\
\ subject with a person of learning there, who was an intimate friend of\n\
\ mine, he said that my face appeared much fairer and smoother when he\n\
\ looked on me from the ground, than it did upon a nearer view, when I took\n\
\ him up in my hand, and brought him close, which he confessed was at first\n\
\ a very shocking sight.  He said, \"he could discover great holes in my\n\
\ skin; that the stumps of my beard were ten times stronger than the\n\
\ bristles of a boar, and my complexion made up of several colours\n\
\ altogether disagreeable:\" although I must beg leave to say for myself,\n\
\ that I am as fair as most of my sex and country, and very little sunburnt\n\
\ by all my travels.  On the other side, discoursing of the ladies in that\n\
\ emperor's court, he used to tell me, \"one had freckles; another too wide\n\
\ a mouth; a third too large a nose;\" nothing of which I was able to\n\
\ distinguish.  I confess this reflection was obvious enough; which,\n\
\ however, I could not forbear, lest the reader might think those vast\n\
\ creatures were actually deformed: for I must do them the justice to say,\n\
\ they are a comely race of people, and particularly the features of my\n\
\ master's countenance, although he was but a farmer, when I beheld him\n\
\ from the height of sixty feet, appeared very well proportioned.\n\
\ \n\
\ When dinner was done, my master went out to his labourers, and, as I\n\
\ could discover by his voice and gesture, gave his wife strict charge to\n\
\ take care of me.  I was very much tired, and disposed to sleep, which my\n\
\ mistress perceiving, she put me on her own bed, and covered me with a\n\
\ clean white handkerchief, but larger and coarser than the mainsail of a\n\
\ man-of-war.\n\
\ \n\
\ I slept about two hours, and dreamt I was at home with my wife and\n\
\ children, which aggravated my sorrows when I awaked, and found myself\n\
\ alone in a vast room, between two and three hundred feet wide, and above\n\
\ two hundred high, lying in a bed twenty yards wide.  My mistress was gone\n\
\ about her household affairs, and had locked me in.  The bed was eight\n\
\ yards from the floor.  Some natural necessities required me to get down;\n\
\ I durst not presume to call; and if I had, it would have been in vain,\n\
\ with such a voice as mine, at so great a distance from the room where I\n\
\ lay to the kitchen where the family kept.  While I was under these\n\
\ circumstances, two rats crept up the curtains, and ran smelling backwards\n\
\ and forwards on the bed.  One of them came up almost to my face,\n\
\ whereupon I rose in a fright, and drew out my hanger to defend myself.\n\
\ These horrible animals had the boldness to attack me on both sides, and\n\
\ one of them held his fore-feet at my collar; but I had the good fortune\n\
\ to rip up his belly before he could do me any mischief.  He fell down at\n\
\ my feet; and the other, seeing the fate of his comrade, made his escape,\n\
\ but not without one good wound on the back, which I gave him as he fled,\n\
\ and made the blood run trickling from him.  After this exploit, I walked\n\
\ gently to and fro on the bed, to recover my breath and loss of spirits.\n\
\ These creatures were of the size of a large mastiff, but infinitely more\n\
\ nimble and fierce; so that if I had taken off my belt before I went to\n\
\ sleep, I must have infallibly been torn to pieces and devoured.  I\n\
\ measured the tail of the dead rat, and found it to be two yards long,\n\
\ wanting an inch; but it went against my stomach to drag the carcass off\n\
\ the bed, where it lay still bleeding; I observed it had yet some life,\n\
\ but with a strong slash across the neck, I thoroughly despatched it.\n\
\ \n\
\ Soon after my mistress came into the room, who seeing me all bloody, ran\n\
\ and took me up in her hand.  I pointed to the dead rat, smiling, and\n\
\ making other signs to show I was not hurt; whereat she was extremely\n\
\ rejoiced, calling the maid to take up the dead rat with a pair of tongs,\n\
\ and throw it out of the window.  Then she set me on a table, where I\n\
\ showed her my hanger all bloody, and wiping it on the lappet of my coat,\n\
\ returned it to the scabbard.  I was pressed to do more than one thing\n\
\ which another could not do for me, and therefore endeavoured to make my\n\
\ mistress understand, that I desired to be set down on the floor; which\n\
\ after she had done, my bashfulness would not suffer me to express myself\n\
\ farther, than by pointing to the door, and bowing several times.  The\n\
\ good woman, with much difficulty, at last perceived what I would be at,\n\
\ and taking me up again in her hand, walked into the garden, where she set\n\
\ me down.  I went on one side about two hundred yards, and beckoning to\n\
\ her not to look or to follow me, I hid myself between two leaves of\n\
\ sorrel, and there discharged the necessities of nature.\n\
\ \n\
\ I hope the gentle reader will excuse me for dwelling on these and the\n\
\ like particulars, which, however insignificant they may appear to\n\
\ groveling vulgar minds, yet will certainly help a philosopher to enlarge\n\
\ his thoughts and imagination, and apply them to the benefit of public as\n\
\ well as private life, which was my sole design in presenting this and\n\
\ other accounts of my travels to the world; wherein I have been chiefly\n\
\ studious of truth, without affecting any ornaments of learning or of\n\
\ style.  But the whole scene of this voyage made so strong an impression\n\
\ on my mind, and is so deeply fixed in my memory, that, in committing it\n\
\ to paper I did not omit one material circumstance: however, upon a strict\n\
\ review, I blotted out several passages.  Of less moment which were in my\n\
\ first copy, for fear of being censured as tedious and trifling, whereof\n\
\ travellers are often, perhaps not without justice, accused.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER II.\n\
\ \n\
\ \n\
\ A description of the farmer's daughter.  The author carried to a\n\
\ market-town, and then to the metropolis.  The particulars of his journey.\n\
\ \n\
\ My mistress had a daughter of nine years old, a child of towardly parts\n\
\ for her age, very dexterous at her needle, and skilful in dressing her\n\
\ baby.  Her mother and she contrived to fit up the baby's cradle for me\n\
\ against night: the cradle was put into a small drawer of a cabinet, and\n\
\ the drawer placed upon a hanging shelf for fear of the rats.  This was my\n\
\ bed all the time I staid with those people, though made more convenient\n\
\ by degrees, as I began to learn their language and make my wants known.\n\
\ This young girl was so handy, that after I had once or twice pulled off\n\
\ my clothes before her, she was able to dress and undress me, though I\n\
\ never gave her that trouble when she would let me do either myself.  She\n\
\ made me seven shirts, and some other linen, of as fine cloth as could be\n\
\ got, which indeed was coarser than sackcloth; and these she constantly\n\
\ washed for me with her own hands.  She was likewise my school-mistress,\n\
\ to teach me the language: when I pointed to any thing, she told me the\n\
\ name of it in her own tongue, so that in a few days I was able to call\n\
\ for whatever I had a mind to.  She was very good-natured, and not above\n\
\ forty feet high, being little for her age.  She gave me the name of\n\
\ _Grildrig_, which the family took up, and afterwards the whole kingdom.\n\
\ The word imports what the Latins call _nanunculus_, the Italians\n\
\ _homunceletino_, and the English _mannikin_.  To her I chiefly owe my\n\
\ preservation in that country: we never parted while I was there; I called\n\
\ her my _Glumdalclitch_, or little nurse; and should be guilty of great\n\
\ ingratitude, if I omitted this honourable mention of her care and\n\
\ affection towards me, which I heartily wish it lay in my power to requite\n\
\ as she deserves, instead of being the innocent, but unhappy instrument of\n\
\ her disgrace, as I have too much reason to fear.\n\
\ \n\
\ It now began to be known and talked of in the neighbourhood, that my\n\
\ master had found a strange animal in the field, about the bigness of a\n\
\ _splacnuck_, but exactly shaped in every part like a human creature;\n\
\ which it likewise imitated in all its actions; seemed to speak in a\n\
\ little language of its own, had already learned several words of theirs,\n\
\ went erect upon two legs, was tame and gentle, would come when it was\n\
\ called, do whatever it was bid, had the finest limbs in the world, and a\n\
\ complexion fairer than a nobleman's daughter of three years old.  Another\n\
\ farmer, who lived hard by, and was a particular friend of my master, came\n\
\ on a visit on purpose to inquire into the truth of this story.  I was\n\
\ immediately produced, and placed upon a table, where I walked as I was\n\
\ commanded, drew my hanger, put it up again, made my reverence to my\n\
\ master's guest, asked him in his own language how he did, and told him\n\
\ _he was welcome_, just as my little nurse had instructed me.  This man,\n\
\ who was old and dim-sighted, put on his spectacles to behold me better;\n\
\ at which I could not forbear laughing very heartily, for his eyes\n\
\ appeared like the full moon shining into a chamber at two windows.  Our\n\
\ people, who discovered the cause of my mirth, bore me company in\n\
\ laughing, at which the old fellow was fool enough to be angry and out of\n\
\ countenance.  He had the character of a great miser; and, to my\n\
\ misfortune, he well deserved it, by the cursed advice he gave my master,\n\
\ to show me as a sight upon a market-day in the next town, which was half\n\
\ an hour's riding, about two-and-twenty miles from our house.  I guessed\n\
\ there was some mischief when I observed my master and his friend\n\
\ whispering together, sometimes pointing at me; and my fears made me fancy\n\
\ that I overheard and understood some of their words.  But the next\n\
\ morning Glumdalclitch, my little nurse, told me the whole matter, which\n\
\ she had cunningly picked out from her mother.  The poor girl laid me on\n\
\ her bosom, and fell a weeping with shame and grief.  She apprehended some\n\
\ mischief would happen to me from rude vulgar folks, who might squeeze me\n\
\ to death, or break one of my limbs by taking me in their hands.  She had\n\
\ also observed how modest I was in my nature, how nicely I regarded my\n\
\ honour, and what an indignity I should conceive it, to be exposed for\n\
\ money as a public spectacle, to the meanest of the people.  She said, her\n\
\ papa and mamma had promised that Grildrig should be hers; but now she\n\
\ found they meant to serve her as they did last year, when they pretended\n\
\ to give her a lamb, and yet, as soon as it was fat, sold it to a butcher.\n\
\ For my own part, I may truly affirm, that I was less concerned than my\n\
\ nurse.  I had a strong hope, which never left me, that I should one day\n\
\ recover my liberty: and as to the ignominy of being carried about for a\n\
\ monster, I considered myself to be a perfect stranger in the country, and\n\
\ that such a misfortune could never be charged upon me as a reproach, if\n\
\ ever I should return to England, since the king of Great Britain himself,\n\
\ in my condition, must have undergone the same distress.\n\
\ \n\
\ My master, pursuant to the advice of his friend, carried me in a box the\n\
\ next market-day to the neighbouring town, and took along with him his\n\
\ little daughter, my nurse, upon a pillion behind him.  The box was close\n\
\ on every side, with a little door for me to go in and out, and a few\n\
\ gimlet holes to let in air.  The girl had been so careful as to put the\n\
\ quilt of her baby's bed into it, for me to lie down on.  However, I was\n\
\ terribly shaken and discomposed in this journey, though it was but of\n\
\ half an hour: for the horse went about forty feet at every step and\n\
\ trotted so high, that the agitation was equal to the rising and falling\n\
\ of a ship in a great storm, but much more frequent.  Our journey was\n\
\ somewhat farther than from London to St. Alban's.  My master alighted at\n\
\ an inn which he used to frequent; and after consulting awhile with the\n\
\ inn-keeper, and making some necessary preparations, he hired the\n\
\ _grultrud_, or crier, to give notice through the town of a strange\n\
\ creature to be seen at the sign of the Green Eagle, not so big as a\n\
\ _splacnuck_ (an animal in that country very finely shaped, about six feet\n\
\ long,) and in every part of the body resembling a human creature, could\n\
\ speak several words, and perform a hundred diverting tricks.\n\
\ \n\
\ I was placed upon a table in the largest room of the inn, which might be\n\
\ near three hundred feet square.  My little nurse stood on a low stool\n\
\ close to the table, to take care of me, and direct what I should do.  My\n\
\ master, to avoid a crowd, would suffer only thirty people at a time to\n\
\ see me.  I walked about on the table as the girl commanded; she asked me\n\
\ questions, as far as she knew my understanding of the language reached,\n\
\ and I answered them as loud as I could.  I turned about several times to\n\
\ the company, paid my humble respects, said _they were welcome_, and used\n\
\ some other speeches I had been taught.  I took up a thimble filled with\n\
\ liquor, which Glumdalclitch had given me for a cup, and drank their\n\
\ health, I drew out my hanger, and flourished with it after the manner of\n\
\ fencers in England.  My nurse gave me a part of a straw, which I\n\
\ exercised as a pike, having learnt the art in my youth.  I was that day\n\
\ shown to twelve sets of company, and as often forced to act over again\n\
\ the same fopperies, till I was half dead with weariness and vexation; for\n\
\ those who had seen me made such wonderful reports, that the people were\n\
\ ready to break down the doors to come in.  My master, for his own\n\
\ interest, would not suffer any one to touch me except my nurse; and to\n\
\ prevent danger, benches were set round the table at such a distance as to\n\
\ put me out of every body's reach.  However, an unlucky school-boy aimed a\n\
\ hazel nut directly at my head, which very narrowly missed me; otherwise\n\
\ it came with so much violence, that it would have infallibly knocked out\n\
\ my brains, for it was almost as large as a small pumpkin, but I had the\n\
\ satisfaction to see the young rogue well beaten, and turned out of the\n\
\ room.\n\
\ \n\
\ My master gave public notice that he would show me again the next\n\
\ market-day; and in the meantime he prepared a convenient vehicle for me,\n\
\ which he had reason enough to do; for I was so tired with my first\n\
\ journey, and with entertaining company for eight hours together, that I\n\
\ could hardly stand upon my legs, or speak a word.  It was at least three\n\
\ days before I recovered my strength; and that I might have no rest at\n\
\ home, all the neighbouring gentlemen from a hundred miles round, hearing\n\
\ of my fame, came to see me at my master's own house.  There could not be\n\
\ fewer than thirty persons with their wives and children (for the country\n\
\ is very populous;) and my master demanded the rate of a full room\n\
\ whenever he showed me at home, although it were only to a single family;\n\
\ so that for some time I had but little ease every day of the week (except\n\
\ Wednesday, which is their Sabbath,) although I were not carried to the\n\
\ town.\n\
\ \n\
\ My master, finding how profitable I was likely to be, resolved to carry\n\
\ me to the most considerable cities of the kingdom.  Having therefore\n\
\ provided himself with all things necessary for a long journey, and\n\
\ settled his affairs at home, he took leave of his wife, and upon the 17th\n\
\ of August, 1703, about two months after my arrival, we set out for the\n\
\ metropolis, situate near the middle of that empire, and about three\n\
\ thousand miles distance from our house.  My master made his daughter\n\
\ Glumdalclitch ride behind him.  She carried me on her lap, in a box tied\n\
\ about her waist.  The girl had lined it on all sides with the softest\n\
\ cloth she could get, well quilted underneath, furnished it with her\n\
\ baby's bed, provided me with linen and other necessaries, and made\n\
\ everything as convenient as she could.  We had no other company but a boy\n\
\ of the house, who rode after us with the luggage.\n\
\ \n\
\ My master's design was to show me in all the towns by the way, and to\n\
\ step out of the road for fifty or a hundred miles, to any village, or\n\
\ person of quality's house, where he might expect custom.  We made easy\n\
\ journeys, of not above seven or eight score miles a-day; for\n\
\ Glumdalclitch, on purpose to spare me, complained she was tired with the\n\
\ trotting of the horse.  She often took me out of my box, at my own\n\
\ desire, to give me air, and show me the country, but always held me fast\n\
\ by a leading-string.  We passed over five or six rivers, many degrees\n\
\ broader and deeper than the Nile or the Ganges: and there was hardly a\n\
\ rivulet so small as the Thames at London-bridge.  We were ten weeks in\n\
\ our journey, and I was shown in eighteen large towns, besides many\n\
\ villages, and private families.\n\
\ \n\
\ On the 26th day of October we arrived at the metropolis, called in their\n\
\ language _Lorbrulgrud_, or Pride of the Universe.  My master took a\n\
\ lodging in the principal street of the city, not far from the royal\n\
\ palace, and put out bills in the usual form, containing an exact\n\
\ description of my person and parts.  He hired a large room between three\n\
\ and four hundred feet wide.  He provided a table sixty feet in diameter,\n\
\ upon which I was to act my part, and pallisadoed it round three feet from\n\
\ the edge, and as many high, to prevent my falling over.  I was shown ten\n\
\ times a-day, to the wonder and satisfaction of all people.  I could now\n\
\ speak the language tolerably well, and perfectly understood every word,\n\
\ that was spoken to me.  Besides, I had learnt their alphabet, and could\n\
\ make a shift to explain a sentence here and there; for Glumdalclitch had\n\
\ been my instructor while we were at home, and at leisure hours during our\n\
\ journey.  She carried a little book in her pocket, not much larger than a\n\
\ Sanson's Atlas; it was a common treatise for the use of young girls,\n\
\ giving a short account of their religion: out of this she taught me my\n\
\ letters, and interpreted the words.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER III.\n\
\ \n\
\ \n\
\ The author sent for to court.  The queen buys him of his master the\n\
\ farmer, and presents him to the king.  He disputes with his majesty's\n\
\ great scholars.  An apartment at court provided for the author.  He is in\n\
\ high favour with the queen.  He stands up for the honour of his own\n\
\ country.  His quarrels with the queen's dwarf.\n\
\ \n\
\ The frequent labours I underwent every day, made, in a few weeks, a very\n\
\ considerable change in my health: the more my master got by me, the more\n\
\ insatiable he grew.  I had quite lost my stomach, and was almost reduced\n\
\ to a skeleton.  The farmer observed it, and concluding I must soon die,\n\
\ resolved to make as good a hand of me as he could.  While he was thus\n\
\ reasoning and resolving with himself, a _sardral_, or gentleman-usher,\n\
\ came from court, commanding my master to carry me immediately thither for\n\
\ the diversion of the queen and her ladies.  Some of the latter had\n\
\ already been to see me, and reported strange things of my beauty,\n\
\ behaviour, and good sense.  Her majesty, and those who attended her, were\n\
\ beyond measure delighted with my demeanour.  I fell on my knees, and\n\
\ begged the honour of kissing her imperial foot; but this gracious\n\
\ princess held out her little finger towards me, after I was set on the\n\
\ table, which I embraced in both my arms, and put the tip of it with the\n\
\ utmost respect to my lip.  She made me some general questions about my\n\
\ country and my travels, which I answered as distinctly, and in as few\n\
\ words as I could.  She asked, \"whether I could be content to live at\n\
\ court?\"  I bowed down to the board of the table, and humbly answered\n\
\ \"that I was my master's slave: but, if I were at my own disposal, I\n\
\ should be proud to devote my life to her majesty's service.\"  She then\n\
\ asked my master, \"whether he was willing to sell me at a good price?\"\n\
\ He, who apprehended I could not live a month, was ready enough to part\n\
\ with me, and demanded a thousand pieces of gold, which were ordered him\n\
\ on the spot, each piece being about the bigness of eight hundred\n\
\ moidores; but allowing for the proportion of all things between that\n\
\ country and Europe, and the high price of gold among them, was hardly so\n\
\ great a sum as a thousand guineas would be in England.  I then said to\n\
\ the queen, \"since I was now her majesty's most humble creature and\n\
\ vassal, I must beg the favour, that Glumdalclitch, who had always tended\n\
\ me with so much care and kindness, and understood to do it so well, might\n\
\ be admitted into her service, and continue to be my nurse and\n\
\ instructor.\"\n\
\ \n\
\ Her majesty agreed to my petition, and easily got the farmer's consent,\n\
\ who was glad enough to have his daughter preferred at court, and the poor\n\
\ girl herself was not able to hide her joy.  My late master withdrew,\n\
\ bidding me farewell, and saying he had left me in a good service; to\n\
\ which I replied not a word, only making him a slight bow.\n\
\ \n\
\ The queen observed my coldness; and, when the farmer was gone out of the\n\
\ apartment, asked me the reason.  I made bold to tell her majesty, \"that I\n\
\ owed no other obligation to my late master, than his not dashing out the\n\
\ brains of a poor harmless creature, found by chance in his fields: which\n\
\ obligation was amply recompensed, by the gain he had made in showing me\n\
\ through half the kingdom, and the price he had now sold me for.  That the\n\
\ life I had since led was laborious enough to kill an animal of ten times\n\
\ my strength.  That my health was much impaired, by the continual drudgery\n\
\ of entertaining the rabble every hour of the day; and that, if my master\n\
\ had not thought my life in danger, her majesty would not have got so\n\
\ cheap a bargain.  But as I was out of all fear of being ill-treated under\n\
\ the protection of so great and good an empress, the ornament of nature,\n\
\ the darling of the world, the delight of her subjects, the phoenix of the\n\
\ creation, so I hoped my late master's apprehensions would appear to be\n\
\ groundless; for I already found my spirits revive, by the influence of\n\
\ her most august presence.\"\n\
\ \n\
\ This was the sum of my speech, delivered with great improprieties and\n\
\ hesitation.  The latter part was altogether framed in the style peculiar\n\
\ to that people, whereof I learned some phrases from Glumdalclitch, while\n\
\ she was carrying me to court.\n\
\ \n\
\ The queen, giving great allowance for my defectiveness in speaking, was,\n\
\ however, surprised at so much wit and good sense in so diminutive an\n\
\ animal.  She took me in her own hand, and carried me to the king, who was\n\
\ then retired to his cabinet.  His majesty, a prince of much gravity and\n\
\ austere countenance, not well observing my shape at first view, asked the\n\
\ queen after a cold manner \"how long it was since she grew fond of a\n\
\ _splacnuck_?\" for such it seems he took me to be, as I lay upon my breast\n\
\ in her majesty's right hand.  But this princess, who has an infinite deal\n\
\ of wit and humour, set me gently on my feet upon the scrutoire, and\n\
\ commanded me to give his majesty an account of myself, which I did in a\n\
\ very few words: and Glumdalclitch who attended at the cabinet door, and\n\
\ could not endure I should be out of her sight, being admitted, confirmed\n\
\ all that had passed from my arrival at her father's house.\n\
\ \n\
\ The king, although he be as learned a person as any in his dominions, had\n\
\ been educated in the study of philosophy, and particularly mathematics;\n\
\ yet when he observed my shape exactly, and saw me walk erect, before I\n\
\ began to speak, conceived I might be a piece of clock-work (which is in\n\
\ that country arrived to a very great perfection) contrived by some\n\
\ ingenious artist.  But when he heard my voice, and found what I delivered\n\
\ to be regular and rational, he could not conceal his astonishment.  He\n\
\ was by no means satisfied with the relation I gave him of the manner I\n\
\ came into his kingdom, but thought it a story concerted between\n\
\ Glumdalclitch and her father, who had taught me a set of words to make me\n\
\ sell at a better price.  Upon this imagination, he put several other\n\
\ questions to me, and still received rational answers: no otherwise\n\
\ defective than by a foreign accent, and an imperfect knowledge in the\n\
\ language, with some rustic phrases which I had learned at the farmer's\n\
\ house, and did not suit the polite style of a court.\n\
\ \n\
\ His majesty sent for three great scholars, who were then in their weekly\n\
\ waiting, according to the custom in that country.  These gentlemen, after\n\
\ they had a while examined my shape with much nicety, were of different\n\
\ opinions concerning me.  They all agreed that I could not be produced\n\
\ according to the regular laws of nature, because I was not framed with a\n\
\ capacity of preserving my life, either by swiftness, or climbing of\n\
\ trees, or digging holes in the earth.  They observed by my teeth, which\n\
\ they viewed with great exactness, that I was a carnivorous animal; yet\n\
\ most quadrupeds being an overmatch for me, and field mice, with some\n\
\ others, too nimble, they could not imagine how I should be able to\n\
\ support myself, unless I fed upon snails and other insects, which they\n\
\ offered, by many learned arguments, to evince that I could not possibly\n\
\ do.  One of these virtuosi seemed to think that I might be an embryo, or\n\
\ abortive birth.  But this opinion was rejected by the other two, who\n\
\ observed my limbs to be perfect and finished; and that I had lived\n\
\ several years, as it was manifest from my beard, the stumps whereof they\n\
\ plainly discovered through a magnifying glass.  They would not allow me\n\
\ to be a dwarf, because my littleness was beyond all degrees of\n\
\ comparison; for the queen's favourite dwarf, the smallest ever known in\n\
\ that kingdom, was near thirty feet high.  After much debate, they\n\
\ concluded unanimously, that I was only _relplum scalcath_, which is\n\
\ interpreted literally _lusus naturae_; a determination exactly agreeable\n\
\ to the modern philosophy of Europe, whose professors, disdaining the old\n\
\ evasion of occult causes, whereby the followers of Aristotle endeavoured\n\
\ in vain to disguise their ignorance, have invented this wonderful\n\
\ solution of all difficulties, to the unspeakable advancement of human\n\
\ knowledge.\n\
\ \n\
\ After this decisive conclusion, I entreated to be heard a word or two.  I\n\
\ applied myself to the king, and assured his majesty, \"that I came from a\n\
\ country which abounded with several millions of both sexes, and of my own\n\
\ stature; where the animals, trees, and houses, were all in proportion,\n\
\ and where, by consequence, I might be as able to defend myself, and to\n\
\ find sustenance, as any of his majesty's subjects could do here; which I\n\
\ took for a full answer to those gentlemen's arguments.\"  To this they\n\
\ only replied with a smile of contempt, saying, \"that the farmer had\n\
\ instructed me very well in my lesson.\"  The king, who had a much better\n\
\ understanding, dismissing his learned men, sent for the farmer, who by\n\
\ good fortune was not yet gone out of town.  Having therefore first\n\
\ examined him privately, and then confronted him with me and the young\n\
\ girl, his majesty began to think that what we told him might possibly be\n\
\ true. He desired the queen to order that a particular care should be\n\
\ taken of me; and was of opinion that Glumdalclitch should still continue\n\
\ in her office of tending me, because he observed we had a great affection\n\
\ for each other.  A convenient apartment was provided for her at court:\n\
\ she had a sort of governess appointed to take care of her education, a\n\
\ maid to dress her, and two other servants for menial offices; but the\n\
\ care of me was wholly appropriated to herself.  The queen commanded her\n\
\ own cabinet-maker to contrive a box, that might serve me for a\n\
\ bedchamber, after the model that Glumdalclitch and I should agree upon.\n\
\ This man was a most ingenious artist, and according to my direction, in\n\
\ three weeks finished for me a wooden chamber of sixteen feet square, and\n\
\ twelve high, with sash-windows, a door, and two closets, like a London\n\
\ bed-chamber.  The board, that made the ceiling, was to be lifted up and\n\
\ down by two hinges, to put in a bed ready furnished by her majesty's\n\
\ upholsterer, which Glumdalclitch took out every day to air, made it with\n\
\ her own hands, and letting it down at night, locked up the roof over me.\n\
\ A nice workman, who was famous for little curiosities, undertook to make\n\
\ me two chairs, with backs and frames, of a substance not unlike ivory,\n\
\ and two tables, with a cabinet to put my things in.  The room was quilted\n\
\ on all sides, as well as the floor and the ceiling, to prevent any\n\
\ accident from the carelessness of those who carried me, and to break the\n\
\ force of a jolt, when I went in a coach.  I desired a lock for my door,\n\
\ to prevent rats and mice from coming in.  The smith, after several\n\
\ attempts, made the smallest that ever was seen among them, for I have\n\
\ known a larger at the gate of a gentleman's house in England.  I made a\n\
\ shift to keep the key in a pocket of my own, fearing Glumdalclitch might\n\
\ lose it.  The queen likewise ordered the thinnest silks that could be\n\
\ gotten, to make me clothes, not much thicker than an English blanket,\n\
\ very cumbersome till I was accustomed to them.  They were after the\n\
\ fashion of the kingdom, partly resembling the Persian, and partly the\n\
\ Chinese, and are a very grave and decent habit.\n\
\ \n\
\ The queen became so fond of my company, that she could not dine without\n\
\ me.  I had a table placed upon the same at which her majesty ate, just at\n\
\ her left elbow, and a chair to sit on.  Glumdalclitch stood on a stool on\n\
\ the floor near my table, to assist and take care of me.  I had an entire\n\
\ set of silver dishes and plates, and other necessaries, which, in\n\
\ proportion to those of the queen, were not much bigger than what I have\n\
\ seen in a London toy-shop for the furniture of a baby-house: these my\n\
\ little nurse kept in her pocket in a silver box, and gave me at meals as\n\
\ I wanted them, always cleaning them herself.  No person dined with the\n\
\ queen but the two princesses royal, the eldest sixteen years old, and the\n\
\ younger at that time thirteen and a month.  Her majesty used to put a bit\n\
\ of meat upon one of my dishes, out of which I carved for myself, and her\n\
\ diversion was to see me eat in miniature: for the queen (who had indeed\n\
\ but a weak stomach) took up, at one mouthful, as much as a dozen English\n\
\ farmers could eat at a meal, which to me was for some time a very\n\
\ nauseous sight.  She would craunch the wing of a lark, bones and all,\n\
\ between her teeth, although it were nine times as large as that of a\n\
\ full-grown turkey; and put a bit of bread into her mouth as big as two\n\
\ twelve-penny loaves.  She drank out of a golden cup, above a hogshead at\n\
\ a draught.  Her knives were twice as long as a scythe, set straight upon\n\
\ the handle.  The spoons, forks, and other instruments, were all in the\n\
\ same proportion.  I remember when Glumdalclitch carried me, out of\n\
\ curiosity, to see some of the tables at court, where ten or a dozen of\n\
\ those enormous knives and forks were lifted up together, I thought I had\n\
\ never till then beheld so terrible a sight.\n\
\ \n\
\ It is the custom, that every Wednesday (which, as I have observed, is\n\
\ their Sabbath) the king and queen, with the royal issue of both sexes,\n\
\ dine together in the apartment of his majesty, to whom I was now become a\n\
\ great favourite; and at these times, my little chair and table were\n\
\ placed at his left hand, before one of the salt-cellars.  This prince\n\
\ took a pleasure in conversing with me, inquiring into the manners,\n\
\ religion, laws, government, and learning of Europe; wherein I gave him\n\
\ the best account I was able.  His apprehension was so clear, and his\n\
\ judgment so exact, that he made very wise reflections and observations\n\
\ upon all I said.  But I confess, that, after I had been a little too\n\
\ copious in talking of my own beloved country, of our trade and wars by\n\
\ sea and land, of our schisms in religion, and parties in the state; the\n\
\ prejudices of his education prevailed so far, that he could not forbear\n\
\ taking me up in his right hand, and stroking me gently with the other,\n\
\ after a hearty fit of laughing, asked me, \"whether I was a whig or tory?\"\n\
\ Then turning to his first minister, who waited behind him with a white\n\
\ staff, near as tall as the mainmast of the Royal Sovereign, he observed\n\
\ \"how contemptible a thing was human grandeur, which could be mimicked by\n\
\ such diminutive insects as I: and yet,\" says he, \"I dare engage these\n\
\ creatures have their titles and distinctions of honour; they contrive\n\
\ little nests and burrows, that they call houses and cities; they make a\n\
\ figure in dress and equipage; they love, they fight, they dispute, they\n\
\ cheat, they betray!\"  And thus he continued on, while my colour came and\n\
\ went several times, with indignation, to hear our noble country, the\n\
\ mistress of arts and arms, the scourge of France, the arbitress of\n\
\ Europe, the seat of virtue, piety, honour, and truth, the pride and envy\n\
\ of the world, so contemptuously treated.\n\
\ \n\
\ But as I was not in a condition to resent injuries, so upon mature\n\
\ thoughts I began to doubt whether I was injured or no.  For, after having\n\
\ been accustomed several months to the sight and converse of this people,\n\
\ and observed every object upon which I cast mine eyes to be of\n\
\ proportionable magnitude, the horror I had at first conceived from their\n\
\ bulk and aspect was so far worn off, that if I had then beheld a company\n\
\ of English lords and ladies in their finery and birth-day clothes, acting\n\
\ their several parts in the most courtly manner of strutting, and bowing,\n\
\ and prating, to say the truth, I should have been strongly tempted to\n\
\ laugh as much at them as the king and his grandees did at me.  Neither,\n\
\ indeed, could I forbear smiling at myself, when the queen used to place\n\
\ me upon her hand towards a looking-glass, by which both our persons\n\
\ appeared before me in full view together; and there could be nothing more\n\
\ ridiculous than the comparison; so that I really began to imagine myself\n\
\ dwindled many degrees below my usual size.\n\
\ \n\
\ Nothing angered and mortified me so much as the queen's dwarf; who being\n\
\ of the lowest stature that was ever in that country (for I verily think\n\
\ he was not full thirty feet high), became so insolent at seeing a\n\
\ creature so much beneath him, that he would always affect to swagger and\n\
\ look big as he passed by me in the queen's antechamber, while I was\n\
\ standing on some table talking with the lords or ladies of the court, and\n\
\ he seldom failed of a smart word or two upon my littleness; against which\n\
\ I could only revenge myself by calling him brother, challenging him to\n\
\ wrestle, and such repartees as are usually in the mouths of court pages.\n\
\ One day, at dinner, this malicious little cub was so nettled with\n\
\ something I had said to him, that, raising himself upon the frame of her\n\
\ majesty's chair, he took me up by the middle, as I was sitting down, not\n\
\ thinking any harm, and let me drop into a large silver bowl of cream, and\n\
\ then ran away as fast as he could.  I fell over head and ears, and, if I\n\
\ had not been a good swimmer, it might have gone very hard with me; for\n\
\ Glumdalclitch in that instant happened to be at the other end of the\n\
\ room, and the queen was in such a fright, that she wanted presence of\n\
\ mind to assist me.  But my little nurse ran to my relief, and took me\n\
\ out, after I had swallowed above a quart of cream.  I was put to bed:\n\
\ however, I received no other damage than the loss of a suit of clothes,\n\
\ which was utterly spoiled.  The dwarf was soundly whipt, and as a farther\n\
\ punishment, forced to drink up the bowl of cream into which he had thrown\n\
\ me: neither was he ever restored to favour; for soon after the queen\n\
\ bestowed him on a lady of high quality, so that I saw him no more, to my\n\
\ very great satisfaction; for I could not tell to what extremities such a\n\
\ malicious urchin might have carried his resentment.\n\
\ \n\
\ He had before served me a scurvy trick, which set the queen a-laughing,\n\
\ although at the same time she was heartily vexed, and would have\n\
\ immediately cashiered him, if I had not been so generous as to intercede.\n\
\ Her majesty had taken a marrow-bone upon her plate, and, after knocking\n\
\ out the marrow, placed the bone again in the dish erect, as it stood\n\
\ before; the dwarf, watching his opportunity, while Glumdalclitch was gone\n\
\ to the side-board, mounted the stool that she stood on to take care of me\n\
\ at meals, took me up in both hands, and squeezing my legs together,\n\
\ wedged them into the marrow bone above my waist, where I stuck for some\n\
\ time, and made a very ridiculous figure.  I believe it was near a minute\n\
\ before any one knew what was become of me; for I thought it below me to\n\
\ cry out.  But, as princes seldom get their meat hot, my legs were not\n\
\ scalded, only my stockings and breeches in a sad condition.  The dwarf,\n\
\ at my entreaty, had no other punishment than a sound whipping.\n\
\ \n\
\ I was frequently rallied by the queen upon account of my fearfulness; and\n\
\ she used to ask me whether the people of my country were as great cowards\n\
\ as myself?  The occasion was this: the kingdom is much pestered with\n\
\ flies in summer; and these odious insects, each of them as big as a\n\
\ Dunstable lark, hardly gave me any rest while I sat at dinner, with their\n\
\ continual humming and buzzing about mine ears.  They would sometimes\n\
\ alight upon my victuals, and leave their loathsome excrement, or spawn\n\
\ behind, which to me was very visible, though not to the natives of that\n\
\ country, whose large optics were not so acute as mine, in viewing smaller\n\
\ objects.  Sometimes they would fix upon my nose, or forehead, where they\n\
\ stung me to the quick, smelling very offensively; and I could easily\n\
\ trace that viscous matter, which, our naturalists tell us, enables those\n\
\ creatures to walk with their feet upwards upon a ceiling.  I had much ado\n\
\ to defend myself against these detestable animals, and could not forbear\n\
\ starting when they came on my face.  It was the common practice of the\n\
\ dwarf, to catch a number of these insects in his hand, as schoolboys do\n\
\ among us, and let them out suddenly under my nose, on purpose to frighten\n\
\ me, and divert the queen.  My remedy was to cut them in pieces with my\n\
\ knife, as they flew in the air, wherein my dexterity was much admired.\n\
\ \n\
\ I remember, one morning, when Glumdalclitch had set me in a box upon a\n\
\ window, as she usually did in fair days to give me air (for I durst not\n\
\ venture to let the box be hung on a nail out of the window, as we do with\n\
\ cages in England), after I had lifted up one of my sashes, and sat down\n\
\ at my table to eat a piece of sweet cake for my breakfast, above twenty\n\
\ wasps, allured by the smell, came flying into the room, humming louder\n\
\ than the drones of as many bagpipes.  Some of them seized my cake, and\n\
\ carried it piecemeal away; others flew about my head and face,\n\
\ confounding me with the noise, and putting me in the utmost terror of\n\
\ their stings.  However, I had the courage to rise and draw my hanger, and\n\
\ attack them in the air.  I dispatched four of them, but the rest got\n\
\ away, and I presently shut my window.  These insects were as large as\n\
\ partridges: I took out their stings, found them an inch and a half long,\n\
\ and as sharp as needles.  I carefully preserved them all; and having\n\
\ since shown them, with some other curiosities, in several parts of\n\
\ Europe, upon my return to England I gave three of them to Gresham\n\
\ College, and kept the fourth for myself.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER IV.\n\
\ \n\
\ \n\
\ The country described.  A proposal for correcting modern maps.  The\n\
\ king's palace; and some account of the metropolis.  The author's way of\n\
\ travelling.  The chief temple described.\n\
\ \n\
\ I now intend to give the reader a short description of this country, as\n\
\ far as I travelled in it, which was not above two thousand miles round\n\
\ Lorbrulgrud, the metropolis.  For the queen, whom I always attended,\n\
\ never went farther when she accompanied the king in his progresses, and\n\
\ there staid till his majesty returned from viewing his frontiers.  The\n\
\ whole extent of this prince's dominions reaches about six thousand miles\n\
\ in length, and from three to five in breadth: whence I cannot but\n\
\ conclude, that our geographers of Europe are in a great error, by\n\
\ supposing nothing but sea between Japan and California; for it was ever\n\
\ my opinion, that there must be a balance of earth to counterpoise the\n\
\ great continent of Tartary; and therefore they ought to correct their\n\
\ maps and charts, by joining this vast tract of land to the north-west\n\
\ parts of America, wherein I shall be ready to lend them my assistance.\n\
\ \n\
\ The kingdom is a peninsula, terminated to the north-east by a ridge of\n\
\ mountains thirty miles high, which are altogether impassable, by reason\n\
\ of the volcanoes upon the tops: neither do the most learned know what\n\
\ sort of mortals inhabit beyond those mountains, or whether they be\n\
\ inhabited at all.  On the three other sides, it is bounded by the ocean.\n\
\ There is not one seaport in the whole kingdom: and those parts of the\n\
\ coasts into which the rivers issue, are so full of pointed rocks, and the\n\
\ sea generally so rough, that there is no venturing with the smallest of\n\
\ their boats; so that these people are wholly excluded from any commerce\n\
\ with the rest of the world.  But the large rivers are full of vessels,\n\
\ and abound with excellent fish; for they seldom get any from the sea,\n\
\ because the sea fish are of the same size with those in Europe, and\n\
\ consequently not worth catching; whereby it is manifest, that nature, in\n\
\ the production of plants and animals of so extraordinary a bulk, is\n\
\ wholly confined to this continent, of which I leave the reasons to be\n\
\ determined by philosophers.  However, now and then they take a whale that\n\
\ happens to be dashed against the rocks, which the common people feed on\n\
\ heartily.  These whales I have known so large, that a man could hardly\n\
\ carry one upon his shoulders; and sometimes, for curiosity, they are\n\
\ brought in hampers to Lorbrulgrud; I saw one of them in a dish at the\n\
\ king's table, which passed for a rarity, but I did not observe he was\n\
\ fond of it; for I think, indeed, the bigness disgusted him, although I\n\
\ have seen one somewhat larger in Greenland.\n\
\ \n\
\ The country is well inhabited, for it contains fifty-one cities, near a\n\
\ hundred walled towns, and a great number of villages.  To satisfy my\n\
\ curious reader, it may be sufficient to describe Lorbrulgrud.  This city\n\
\ stands upon almost two equal parts, on each side the river that passes\n\
\ through.  It contains above eighty thousand houses, and about six hundred\n\
\ thousand inhabitants.  It is in length three _glomglungs_ (which make\n\
\ about fifty-four English miles,) and two and a half in breadth; as I\n\
\ measured it myself in the royal map made by the king's order, which was\n\
\ laid on the ground on purpose for me, and extended a hundred feet: I\n\
\ paced the diameter and circumference several times barefoot, and,\n\
\ computing by the scale, measured it pretty exactly.\n\
\ \n\
\ The king's palace is no regular edifice, but a heap of buildings, about\n\
\ seven miles round: the chief rooms are generally two hundred and forty\n\
\ feet high, and broad and long in proportion.  A coach was allowed to\n\
\ Glumdalclitch and me, wherein her governess frequently took her out to\n\
\ see the town, or go among the shops; and I was always of the party,\n\
\ carried in my box; although the girl, at my own desire, would often take\n\
\ me out, and hold me in her hand, that I might more conveniently view the\n\
\ houses and the people, as we passed along the streets.  I reckoned our\n\
\ coach to be about a square of Westminster-hall, but not altogether so\n\
\ high: however, I cannot be very exact.  One day the governess ordered our\n\
\ coachman to stop at several shops, where the beggars, watching their\n\
\ opportunity, crowded to the sides of the coach, and gave me the most\n\
\ horrible spectacle that ever a European eye beheld.  There was a woman\n\
\ with a cancer in her breast, swelled to a monstrous size, full of holes,\n\
\ in two or three of which I could have easily crept, and covered my whole\n\
\ body.  There was a fellow with a wen in his neck, larger than five\n\
\ wool-packs; and another, with a couple of wooden legs, each about twenty\n\
\ feet high.  But the most hateful sight of all, was the lice crawling on\n\
\ their clothes.  I could see distinctly the limbs of these vermin with my\n\
\ naked eye, much better than those of a European louse through a\n\
\ microscope, and their snouts with which they rooted like swine.  They\n\
\ were the first I had ever beheld, and I should have been curious enough\n\
\ to dissect one of them, if I had had proper instruments, which I\n\
\ unluckily left behind me in the ship, although, indeed, the sight was so\n\
\ nauseous, that it perfectly turned my stomach.\n\
\ \n\
\ Besides the large box in which I was usually carried, the queen ordered a\n\
\ smaller one to be made for me, of about twelve feet square, and ten high,\n\
\ for the convenience of travelling; because the other was somewhat too\n\
\ large for Glumdalclitch's lap, and cumbersome in the coach; it was made\n\
\ by the same artist, whom I directed in the whole contrivance.  This\n\
\ travelling-closet was an exact square, with a window in the middle of\n\
\ three of the squares, and each window was latticed with iron wire on the\n\
\ outside, to prevent accidents in long journeys.  On the fourth side,\n\
\ which had no window, two strong staples were fixed, through which the\n\
\ person that carried me, when I had a mind to be on horseback, put a\n\
\ leathern belt, and buckled it about his waist.  This was always the\n\
\ office of some grave trusty servant, in whom I could confide, whether I\n\
\ attended the king and queen in their progresses, or were disposed to see\n\
\ the gardens, or pay a visit to some great lady or minister of state in\n\
\ the court, when Glumdalclitch happened to be out of order; for I soon\n\
\ began to be known and esteemed among the greatest officers, I suppose\n\
\ more upon account of their majesties' favour, than any merit of my own.\n\
\ In journeys, when I was weary of the coach, a servant on horseback would\n\
\ buckle on my box, and place it upon a cushion before him; and there I had\n\
\ a full prospect of the country on three sides, from my three windows.  I\n\
\ had, in this closet, a field-bed and a hammock, hung from the ceiling,\n\
\ two chairs and a table, neatly screwed to the floor, to prevent being\n\
\ tossed about by the agitation of the horse or the coach.  And having been\n\
\ long used to sea-voyages, those motions, although sometimes very violent,\n\
\ did not much discompose me.\n\
\ \n\
\ Whenever I had a mind to see the town, it was always in my\n\
\ travelling-closet; which Glumdalclitch held in her lap in a kind of open\n\
\ sedan, after the fashion of the country, borne by four men, and attended\n\
\ by two others in the queen's livery.  The people, who had often heard of\n\
\ me, were very curious to crowd about the sedan, and the girl was\n\
\ complaisant enough to make the bearers stop, and to take me in her hand,\n\
\ that I might be more conveniently seen.\n\
\ \n\
\ I was very desirous to see the chief temple, and particularly the tower\n\
\ belonging to it, which is reckoned the highest in the kingdom.\n\
\ Accordingly one day my nurse carried me thither, but I may truly say I\n\
\ came back disappointed; for the height is not above three thousand feet,\n\
\ reckoning from the ground to the highest pinnacle top; which, allowing\n\
\ for the difference between the size of those people and us in Europe, is\n\
\ no great matter for admiration, nor at all equal in proportion (if I\n\
\ rightly remember) to Salisbury steeple.  But, not to detract from a\n\
\ nation, to which, during my life, I shall acknowledge myself extremely\n\
\ obliged, it must be allowed, that whatever this famous tower wants in\n\
\ height, is amply made up in beauty and strength: for the walls are near a\n\
\ hundred feet thick, built of hewn stone, whereof each is about forty feet\n\
\ square, and adorned on all sides with statues of gods and emperors, cut\n\
\ in marble, larger than the life, placed in their several niches.  I\n\
\ measured a little finger which had fallen down from one of these statues,\n\
\ and lay unperceived among some rubbish, and found it exactly four feet\n\
\ and an inch in length.  Glumdalclitch wrapped it up in her handkerchief,\n\
\ and carried it home in her pocket, to keep among other trinkets, of which\n\
\ the girl was very fond, as children at her age usually are.\n\
\ \n\
\ The king's kitchen is indeed a noble building, vaulted at top, and about\n\
\ six hundred feet high.  The great oven is not so wide, by ten paces, as\n\
\ the cupola at St. Paul's: for I measured the latter on purpose, after my\n\
\ return.  But if I should describe the kitchen grate, the prodigious pots\n\
\ and kettles, the joints of meat turning on the spits, with many other\n\
\ particulars, perhaps I should be hardly believed; at least a severe\n\
\ critic would be apt to think I enlarged a little, as travellers are often\n\
\ suspected to do.  To avoid which censure I fear I have run too much into\n\
\ the other extreme; and that if this treatise should happen to be\n\
\ translated into the language of Brobdingnag (which is the general name of\n\
\ that kingdom,) and transmitted thither, the king and his people would\n\
\ have reason to complain that I had done them an injury, by a false and\n\
\ diminutive representation.\n\
\ \n\
\ His majesty seldom keeps above six hundred horses in his stables: they\n\
\ are generally from fifty-four to sixty feet high.  But, when he goes\n\
\ abroad on solemn days, he is attended, for state, by a military guard of\n\
\ five hundred horse, which, indeed, I thought was the most splendid sight\n\
\ that could be ever beheld, till I saw part of his army in battalia,\n\
\ whereof I shall find another occasion to speak.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER V.\n\
\ \n\
\ \n\
\ Several adventurers that happened to the author.  The execution of a\n\
\ criminal.  The author shows his skill in navigation.\n\
\ \n\
\ I should have lived happy enough in that country, if my littleness had\n\
\ not exposed me to several ridiculous and troublesome accidents; some of\n\
\ which I shall venture to relate.  Glumdalclitch often carried me into the\n\
\ gardens of the court in my smaller box, and would sometimes take me out\n\
\ of it, and hold me in her hand, or set me down to walk.  I remember,\n\
\ before the dwarf left the queen, he followed us one day into those\n\
\ gardens, and my nurse having set me down, he and I being close together,\n\
\ near some dwarf apple trees, I must needs show my wit, by a silly\n\
\ allusion between him and the trees, which happens to hold in their\n\
\ language as it does in ours.  Whereupon, the malicious rogue, watching\n\
\ his opportunity, when I was walking under one of them, shook it directly\n\
\ over my head, by which a dozen apples, each of them near as large as a\n\
\ Bristol barrel, came tumbling about my ears; one of them hit me on the\n\
\ back as I chanced to stoop, and knocked me down flat on my face; but I\n\
\ received no other hurt, and the dwarf was pardoned at my desire, because\n\
\ I had given the provocation.\n\
\ \n\
\ Another day, Glumdalclitch left me on a smooth grass-plot to divert\n\
\ myself, while she walked at some distance with her governess.  In the\n\
\ meantime, there suddenly fell such a violent shower of hail, that I was\n\
\ immediately by the force of it, struck to the ground: and when I was\n\
\ down, the hailstones gave me such cruel bangs all over the body, as if I\n\
\ had been pelted with tennis-balls; however, I made a shift to creep on\n\
\ all fours, and shelter myself, by lying flat on my face, on the lee-side\n\
\ of a border of lemon-thyme, but so bruised from head to foot, that I\n\
\ could not go abroad in ten days.  Neither is this at all to be wondered\n\
\ at, because nature, in that country, observing the same proportion\n\
\ through all her operations, a hailstone is near eighteen hundred times as\n\
\ large as one in Europe; which I can assert upon experience, having been\n\
\ so curious as to weigh and measure them.\n\
\ \n\
\ But a more dangerous accident happened to me in the same garden, when my\n\
\ little nurse, believing she had put me in a secure place (which I often\n\
\ entreated her to do, that I might enjoy my own thoughts,) and having left\n\
\ my box at home, to avoid the trouble of carrying it, went to another part\n\
\ of the garden with her governess and some ladies of her acquaintance.\n\
\ While she was absent, and out of hearing, a small white spaniel that\n\
\ belonged to one of the chief gardeners, having got by accident into the\n\
\ garden, happened to range near the place where I lay: the dog, following\n\
\ the scent, came directly up, and taking me in his mouth, ran straight to\n\
\ his master wagging his tail, and set me gently on the ground.  By good\n\
\ fortune he had been so well taught, that I was carried between his teeth\n\
\ without the least hurt, or even tearing my clothes.  But the poor\n\
\ gardener, who knew me well, and had a great kindness for me, was in a\n\
\ terrible fright: he gently took me up in both his hands, and asked me how\n\
\ I did? but I was so amazed and out of breath, that I could not speak a\n\
\ word.  In a few minutes I came to myself, and he carried me safe to my\n\
\ little nurse, who, by this time, had returned to the place where she left\n\
\ me, and was in cruel agonies when I did not appear, nor answer when she\n\
\ called.  She severely reprimanded the gardener on account of his dog.\n\
\ But the thing was hushed up, and never known at court, for the girl was\n\
\ afraid of the queen's anger; and truly, as to myself, I thought it would\n\
\ not be for my reputation, that such a story should go about.\n\
\ \n\
\ This accident absolutely determined Glumdalclitch never to trust me\n\
\ abroad for the future out of her sight.  I had been long afraid of this\n\
\ resolution, and therefore concealed from her some little unlucky\n\
\ adventures, that happened in those times when I was left by myself.  Once\n\
\ a kite, hovering over the garden, made a stoop at me, and if I had not\n\
\ resolutely drawn my hanger, and run under a thick espalier, he would have\n\
\ certainly carried me away in his talons.  Another time, walking to the\n\
\ top of a fresh mole-hill, I fell to my neck in the hole, through which\n\
\ that animal had cast up the earth, and coined some lie, not worth\n\
\ remembering, to excuse myself for spoiling my clothes.  I likewise broke\n\
\ my right shin against the shell of a snail, which I happened to stumble\n\
\ over, as I was walking alone and thinking on poor England.\n\
\ \n\
\ I cannot tell whether I were more pleased or mortified to observe, in\n\
\ those solitary walks, that the smaller birds did not appear to be at all\n\
\ afraid of me, but would hop about within a yard's distance, looking for\n\
\ worms and other food, with as much indifference and security as if no\n\
\ creature at all were near them.  I remember, a thrush had the confidence\n\
\ to snatch out of my hand, with his bill, a of cake that Glumdalclitch had\n\
\ just given me for my breakfast.  When I attempted to catch any of these\n\
\ birds, they would boldly turn against me, endeavouring to peck my\n\
\ fingers, which I durst not venture within their reach; and then they\n\
\ would hop back unconcerned, to hunt for worms or snails, as they did\n\
\ before.  But one day, I took a thick cudgel, and threw it with all my\n\
\ strength so luckily, at a linnet, that I knocked him down, and seizing\n\
\ him by the neck with both my hands, ran with him in triumph to my nurse.\n\
\ However, the bird, who had only been stunned, recovering himself gave me\n\
\ so many boxes with his wings, on both sides of my head and body, though I\n\
\ held him at arm's-length, and was out of the reach of his claws, that I\n\
\ was twenty times thinking to let him go.  But I was soon relieved by one\n\
\ of our servants, who wrung off the bird's neck, and I had him next day\n\
\ for dinner, by the queen's command.  This linnet, as near as I can\n\
\ remember, seemed to be somewhat larger than an English swan.\n\
\ \n\
\ The maids of honour often invited Glumdalclitch to their apartments, and\n\
\ desired she would bring me along with her, on purpose to have the\n\
\ pleasure of seeing and touching me.  They would often strip me naked from\n\
\ top to toe, and lay me at full length in their bosoms; wherewith I was\n\
\ much disgusted because, to say the truth, a very offensive smell came\n\
\ from their skins; which I do not mention, or intend, to the disadvantage\n\
\ of those excellent ladies, for whom I have all manner of respect; but I\n\
\ conceive that my sense was more acute in proportion to my littleness, and\n\
\ that those illustrious persons were no more disagreeable to their lovers,\n\
\ or to each other, than people of the same quality are with us in England.\n\
\ And, after all, I found their natural smell was much more supportable,\n\
\ than when they used perfumes, under which I immediately swooned away.  I\n\
\ cannot forget, that an intimate friend of mine in Lilliput, took the\n\
\ freedom in a warm day, when I had used a good deal of exercise, to\n\
\ complain of a strong smell about me, although I am as little faulty that\n\
\ way, as most of my sex: but I suppose his faculty of smelling was as nice\n\
\ with regard to me, as mine was to that of this people.  Upon this point,\n\
\ I cannot forbear doing justice to the queen my mistress, and\n\
\ Glumdalclitch my nurse, whose persons were as sweet as those of any lady\n\
\ in England.\n\
\ \n\
\ That which gave me most uneasiness among these maids of honour (when my\n\
\ nurse carried me to visit then) was, to see them use me without any\n\
\ manner of ceremony, like a creature who had no sort of consequence: for\n\
\ they would strip themselves to the skin, and put on their smocks in my\n\
\ presence, while I was placed on their toilet, directly before their naked\n\
\ bodies, which I am sure to me was very far from being a tempting sight,\n\
\ or from giving me any other emotions than those of horror and disgust:\n\
\ their skins appeared so coarse and uneven, so variously coloured, when I\n\
\ saw them near, with a mole here and there as broad as a trencher, and\n\
\ hairs hanging from it thicker than packthreads, to say nothing farther\n\
\ concerning the rest of their persons.  Neither did they at all scruple,\n\
\ while I was by, to discharge what they had drank, to the quantity of at\n\
\ least two hogsheads, in a vessel that held above three tuns.  The\n\
\ handsomest among these maids of honour, a pleasant, frolicsome girl of\n\
\ sixteen, would sometimes set me astride upon one of her nipples, with\n\
\ many other tricks, wherein the reader will excuse me for not being over\n\
\ particular.  But I was so much displeased, that I entreated Glumdalclitch\n\
\ to contrive some excuse for not seeing that young lady any more.\n\
\ \n\
\ One day, a young gentleman, who was nephew to my nurse's governess, came\n\
\ and pressed them both to see an execution.  It was of a man, who had\n\
\ murdered one of that gentleman's intimate acquaintance.  Glumdalclitch\n\
\ was prevailed on to be of the company, very much against her inclination,\n\
\ for she was naturally tender-hearted: and, as for myself, although I\n\
\ abhorred such kind of spectacles, yet my curiosity tempted me to see\n\
\ something that I thought must be extraordinary.  The malefactor was fixed\n\
\ in a chair upon a scaffold erected for that purpose, and his head cut off\n\
\ at one blow, with a sword of about forty feet long.  The veins and\n\
\ arteries spouted up such a prodigious quantity of blood, and so high in\n\
\ the air, that the great _jet d'eau_ at Versailles was not equal to it for\n\
\ the time it lasted: and the head, when it fell on the scaffold floor,\n\
\ gave such a bounce as made me start, although I was at least half an\n\
\ English mile distant.\n\
\ \n\
\ The queen, who often used to hear me talk of my sea-voyages, and took all\n\
\ occasions to divert me when I was melancholy, asked me whether I\n\
\ understood how to handle a sail or an oar, and whether a little exercise\n\
\ of rowing might not be convenient for my health?  I answered, that I\n\
\ understood both very well: for although my proper employment had been to\n\
\ be surgeon or doctor to the ship, yet often, upon a pinch, I was forced\n\
\ to work like a common mariner.  But I could not see how this could be\n\
\ done in their country, where the smallest wherry was equal to a\n\
\ first-rate man of war among us; and such a boat as I could manage would\n\
\ never live in any of their rivers.  Her majesty said, if I would contrive\n\
\ a boat, her own joiner should make it, and she would provide a place for\n\
\ me to sail in.  The fellow was an ingenious workman, and by my\n\
\ instructions, in ten days, finished a pleasure-boat with all its\n\
\ tackling, able conveniently to hold eight Europeans.  When it was\n\
\ finished, the queen was so delighted, that she ran with it in her lap to\n\
\ the king, who ordered it to be put into a cistern full of water, with me\n\
\ in it, by way of trial, where I could not manage my two sculls, or little\n\
\ oars, for want of room.  But the queen had before contrived another\n\
\ project.  She ordered the joiner to make a wooden trough of three hundred\n\
\ feet long, fifty broad, and eight deep; which, being well pitched, to\n\
\ prevent leaking, was placed on the floor, along the wall, in an outer\n\
\ room of the palace.  It had a cock near the bottom to let out the water,\n\
\ when it began to grow stale; and two servants could easily fill it in\n\
\ half an hour.  Here I often used to row for my own diversion, as well as\n\
\ that of the queen and her ladies, who thought themselves well entertained\n\
\ with my skill and agility.  Sometimes I would put up my sail, and then my\n\
\ business was only to steer, while the ladies gave me a gale with their\n\
\ fans; and, when they were weary, some of their pages would blow my sail\n\
\ forward with their breath, while I showed my art by steering starboard or\n\
\ larboard as I pleased.  When I had done, Glumdalclitch always carried\n\
\ back my boat into her closet, and hung it on a nail to dry.\n\
\ \n\
\ In this exercise I once met an accident, which had like to have cost me\n\
\ my life; for, one of the pages having put my boat into the trough, the\n\
\ governess who attended Glumdalclitch very officiously lifted me up, to\n\
\ place me in the boat: but I happened to slip through her fingers, and\n\
\ should infallibly have fallen down forty feet upon the floor, if, by the\n\
\ luckiest chance in the world, I had not been stopped by a corking-pin\n\
\ that stuck in the good gentlewoman's stomacher; the head of the pin\n\
\ passing between my shirt and the waistband of my breeches, and thus I was\n\
\ held by the middle in the air, till Glumdalclitch ran to my relief.\n\
\ \n\
\ Another time, one of the servants, whose office it was to fill my trough\n\
\ every third day with fresh water, was so careless as to let a huge frog\n\
\ (not perceiving it) slip out of his pail.  The frog lay concealed till I\n\
\ was put into my boat, but then, seeing a resting-place, climbed up, and\n\
\ made it lean so much on one side, that I was forced to balance it with\n\
\ all my weight on the other, to prevent overturning.  When the frog was\n\
\ got in, it hopped at once half the length of the boat, and then over my\n\
\ head, backward and forward, daubing my face and clothes with its odious\n\
\ slime.  The largeness of its features made it appear the most deformed\n\
\ animal that can be conceived.  However, I desired Glumdalclitch to let me\n\
\ deal with it alone.  I banged it a good while with one of my sculls, and\n\
\ at last forced it to leap out of the boat.\n\
\ \n\
\ But the greatest danger I ever underwent in that kingdom, was from a\n\
\ monkey, who belonged to one of the clerks of the kitchen.  Glumdalclitch\n\
\ had locked me up in her closet, while she went somewhere upon business,\n\
\ or a visit.  The weather being very warm, the closet-window was left\n\
\ open, as well as the windows and the door of my bigger box, in which I\n\
\ usually lived, because of its largeness and conveniency.  As I sat\n\
\ quietly meditating at my table, I heard something bounce in at the\n\
\ closet-window, and skip about from one side to the other: whereat,\n\
\ although I was much alarmed, yet I ventured to look out, but not stirring\n\
\ from my seat; and then I saw this frolicsome animal frisking and leaping\n\
\ up and down, till at last he came to my box, which he seemed to view with\n\
\ great pleasure and curiosity, peeping in at the door and every window.  I\n\
\ retreated to the farther corner of my room; or box; but the monkey\n\
\ looking in at every side, put me in such a fright, that I wanted presence\n\
\ of mind to conceal myself under the bed, as I might easily have done.\n\
\ After some time spent in peeping, grinning, and chattering, he at last\n\
\ espied me; and reaching one of his paws in at the door, as a cat does\n\
\ when she plays with a mouse, although I often shifted place to avoid him,\n\
\ he at length seized the lappet of my coat (which being made of that\n\
\ country silk, was very thick and strong), and dragged me out.  He took me\n\
\ up in his right fore-foot and held me as a nurse does a child she is\n\
\ going to suckle, just as I have seen the same sort of creature do with a\n\
\ kitten in Europe; and when I offered to struggle he squeezed me so hard,\n\
\ that I thought it more prudent to submit.  I have good reason to believe,\n\
\ that he took me for a young one of his own species, by his often stroking\n\
\ my face very gently with his other paw.  In these diversions he was\n\
\ interrupted by a noise at the closet door, as if somebody were opening\n\
\ it: whereupon he suddenly leaped up to the window at which he had come\n\
\ in, and thence upon the leads and gutters, walking upon three legs, and\n\
\ holding me in the fourth, till he clambered up to a roof that was next to\n\
\ ours.  I heard Glumdalclitch give a shriek at the moment he was carrying\n\
\ me out.  The poor girl was almost distracted: that quarter of the palace\n\
\ was all in an uproar; the servants ran for ladders; the monkey was seen\n\
\ by hundreds in the court, sitting upon the ridge of a building, holding\n\
\ me like a baby in one of his forepaws, and feeding me with the other, by\n\
\ cramming into my mouth some victuals he had squeezed out of the bag on\n\
\ one side of his chaps, and patting me when I would not eat; whereat many\n\
\ of the rabble below could not forbear laughing; neither do I think they\n\
\ justly ought to be blamed, for, without question, the sight was\n\
\ ridiculous enough to every body but myself.  Some of the people threw up\n\
\ stones, hoping to drive the monkey down; but this was strictly forbidden,\n\
\ or else, very probably, my brains had been dashed out.\n\
\ \n\
\ The ladders were now applied, and mounted by several men; which the\n\
\ monkey observing, and finding himself almost encompassed, not being able\n\
\ to make speed enough with his three legs, let me drop on a ridge tile,\n\
\ and made his escape.  Here I sat for some time, five hundred yards from\n\
\ the ground, expecting every moment to be blown down by the wind, or to\n\
\ fall by my own giddiness, and come tumbling over and over from the ridge\n\
\ to the eaves; but an honest lad, one of my nurse's footmen, climbed up,\n\
\ and putting me into his breeches pocket, brought me down safe.\n\
\ \n\
\ I was almost choked with the filthy stuff the monkey had crammed down my\n\
\ throat: but my dear little nurse picked it out of my mouth with a small\n\
\ needle, and then I fell a-vomiting, which gave me great relief.  Yet I\n\
\ was so weak and bruised in the sides with the squeezes given me by this\n\
\ odious animal, that I was forced to keep my bed a fortnight.  The king,\n\
\ queen, and all the court, sent every day to inquire after my health; and\n\
\ her majesty made me several visits during my sickness.  The monkey was\n\
\ killed, and an order made, that no such animal should be kept about the\n\
\ palace.\n\
\ \n\
\ When I attended the king after my recovery, to return him thanks for his\n\
\ favours, he was pleased to rally me a good deal upon this adventure.  He\n\
\ asked me, \"what my thoughts and speculations were, while I lay in the\n\
\ monkey's paw; how I liked the victuals he gave me; his manner of feeding;\n\
\ and whether the fresh air on the roof had sharpened my stomach.\"  He\n\
\ desired to know, \"what I would have done upon such an occasion in my own\n\
\ country.\"  I told his majesty, \"that in Europe we had no monkeys, except\n\
\ such as were brought for curiosity from other places, and so small, that\n\
\ I could deal with a dozen of them together, if they presumed to attack\n\
\ me.  And as for that monstrous animal with whom I was so lately engaged\n\
\ (it was indeed as large as an elephant), if my fears had suffered me to\n\
\ think so far as to make use of my hanger,\" (looking fiercely, and\n\
\ clapping my hand on the hilt, as I spoke) \"when he poked his paw into my\n\
\ chamber, perhaps I should have given him such a wound, as would have made\n\
\ him glad to withdraw it with more haste than he put it in.\"  This I\n\
\ delivered in a firm tone, like a person who was jealous lest his courage\n\
\ should be called in question.  However, my speech produced nothing else\n\
\ beside a laud laughter, which all the respect due to his majesty from\n\
\ those about him could not make them contain.  This made me reflect, how\n\
\ vain an attempt it is for a man to endeavour to do himself honour among\n\
\ those who are out of all degree of equality or comparison with him.  And\n\
\ yet I have seen the moral of my own behaviour very frequent in England\n\
\ since my return; where a little contemptible varlet, without the least\n\
\ title to birth, person, wit, or common sense, shall presume to look with\n\
\ importance, and put himself upon a foot with the greatest persons of the\n\
\ kingdom.\n\
\ \n\
\ I was every day furnishing the court with some ridiculous story: and\n\
\ Glumdalclitch, although she loved me to excess, yet was arch enough to\n\
\ inform the queen, whenever I committed any folly that she thought would\n\
\ be diverting to her majesty.  The girl, who had been out of order, was\n\
\ carried by her governess to take the air about an hour's distance, or\n\
\ thirty miles from town.  They alighted out of the coach near a small\n\
\ foot-path in a field, and Glumdalclitch setting down my travelling box, I\n\
\ went out of it to walk.  There was a cow-dung in the path, and I must\n\
\ need try my activity by attempting to leap over it.  I took a run, but\n\
\ unfortunately jumped short, and found myself just in the middle up to my\n\
\ knees.  I waded through with some difficulty, and one of the footmen\n\
\ wiped me as clean as he could with his handkerchief, for I was filthily\n\
\ bemired; and my nurse confined me to my box, till we returned home; where\n\
\ the queen was soon informed of what had passed, and the footmen spread it\n\
\ about the court: so that all the mirth for some days was at my expense.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER VI.\n\
\ \n\
\ \n\
\ Several contrivances of the author to please the king and queen.  He\n\
\ shows his skill in music.  The king inquires into the state of England,\n\
\ which the author relates to him.  The king's observations thereon.\n\
\ \n\
\ I used to attend the king's levee once or twice a week, and had often\n\
\ seen him under the barber's hand, which indeed was at first very terrible\n\
\ to behold; for the razor was almost twice as long as an ordinary scythe.\n\
\ His majesty, according to the custom of the country, was only shaved\n\
\ twice a-week.  I once prevailed on the barber to give me some of the suds\n\
\ or lather, out of which I picked forty or fifty of the strongest stumps\n\
\ of hair.  I then took a piece of fine wood, and cut it like the back of a\n\
\ comb, making several holes in it at equal distances with as small a\n\
\ needle as I could get from Glumdalclitch.  I fixed in the stumps so\n\
\ artificially, scraping and sloping them with my knife toward the points,\n\
\ that I made a very tolerable comb; which was a seasonable supply, my own\n\
\ being so much broken in the teeth, that it was almost useless: neither\n\
\ did I know any artist in that country so nice and exact, as would\n\
\ undertake to make me another.\n\
\ \n\
\ And this puts me in mind of an amusement, wherein I spent many of my\n\
\ leisure hours.  I desired the queen's woman to save for me the combings\n\
\ of her majesty's hair, whereof in time I got a good quantity; and\n\
\ consulting with my friend the cabinet-maker, who had received general\n\
\ orders to do little jobs for me, I directed him to make two chair-frames,\n\
\ no larger than those I had in my box, and to bore little holes with a\n\
\ fine awl, round those parts where I designed the backs and seats; through\n\
\ these holes I wove the strongest hairs I could pick out, just after the\n\
\ manner of cane chairs in England.  When they were finished, I made a\n\
\ present of them to her majesty; who kept them in her cabinet, and used to\n\
\ show them for curiosities, as indeed they were the wonder of every one\n\
\ that beheld them.  The queen would have me sit upon one of these chairs,\n\
\ but I absolutely refused to obey her, protesting I would rather die than\n\
\ place a dishonourable part of my body on those precious hairs, that once\n\
\ adorned her majesty's head.  Of these hairs (as I had always a mechanical\n\
\ genius) I likewise made a neat little purse, about five feet long, with\n\
\ her majesty's name deciphered in gold letters, which I gave to\n\
\ Glumdalclitch, by the queen's consent.  To say the truth, it was more for\n\
\ show than use, being not of strength to bear the weight of the larger\n\
\ coins, and therefore she kept nothing in it but some little toys that\n\
\ girls are fond of.\n\
\ \n\
\ The king, who delighted in music, had frequent concerts at court, to\n\
\ which I was sometimes carried, and set in my box on a table to hear them:\n\
\ but the noise was so great that I could hardly distinguish the tunes.  I\n\
\ am confident that all the drums and trumpets of a royal army, beating and\n\
\ sounding together just at your ears, could not equal it.  My practice was\n\
\ to have my box removed from the place where the performers sat, as far as\n\
\ I could, then to shut the doors and windows of it, and draw the window\n\
\ curtains; after which I found their music not disagreeable.\n\
\ \n\
\ I had learned in my youth to play a little upon the spinet.\n\
\ Glumdalclitch kept one in her chamber, and a master attended twice a-week\n\
\ to teach her: I called it a spinet, because it somewhat resembled that\n\
\ instrument, and was played upon in the same manner.  A fancy came into my\n\
\ head, that I would entertain the king and queen with an English tune upon\n\
\ this instrument.  But this appeared extremely difficult: for the spinet\n\
\ was near sixty feet long, each key being almost a foot wide, so that with\n\
\ my arms extended I could not reach to above five keys, and to press them\n\
\ down required a good smart stroke with my fist, which would be too great\n\
\ a labour, and to no purpose.  The method I contrived was this: I prepared\n\
\ two round sticks, about the bigness of common cudgels; they were thicker\n\
\ at one end than the other, and I covered the thicker ends with pieces of\n\
\ a mouse's skin, that by rapping on them I might neither damage the tops\n\
\ of the keys nor interrupt the sound.  Before the spinet a bench was\n\
\ placed, about four feet below the keys, and I was put upon the bench.  I\n\
\ ran sideling upon it, that way and this, as fast as I could, banging the\n\
\ proper keys with my two sticks, and made a shift to play a jig, to the\n\
\ great satisfaction of both their majesties; but it was the most violent\n\
\ exercise I ever underwent; and yet I could not strike above sixteen keys,\n\
\ nor consequently play the bass and treble together, as other artists do;\n\
\ which was a great disadvantage to my performance.\n\
\ \n\
\ The king, who, as I before observed, was a prince of excellent\n\
\ understanding, would frequently order that I should be brought in my box,\n\
\ and set upon the table in his closet: he would then command me to bring\n\
\ one of my chairs out of the box, and sit down within three yards distance\n\
\ upon the top of the cabinet, which brought me almost to a level with his\n\
\ face.  In this manner I had several conversations with him.  I one day\n\
\ took the freedom to tell his majesty, \"that the contempt he discovered\n\
\ towards Europe, and the rest of the world, did not seem answerable to\n\
\ those excellent qualities of mind that he was master of; that reason did\n\
\ not extend itself with the bulk of the body; on the contrary, we observed\n\
\ in our country, that the tallest persons were usually the least provided\n\
\ with it; that among other animals, bees and ants had the reputation of\n\
\ more industry, art, and sagacity, than many of the larger kinds; and\n\
\ that, as inconsiderable as he took me to be, I hoped I might live to do\n\
\ his majesty some signal service.\"  The king heard me with attention, and\n\
\ began to conceive a much better opinion of me than he had ever before.\n\
\ He desired \"I would give him as exact an account of the government of\n\
\ England as I possibly could; because, as fond as princes commonly are of\n\
\ their own customs (for so he conjectured of other monarchs, by my former\n\
\ discourses), he should be glad to hear of any thing that might deserve\n\
\ imitation.\"\n\
\ \n\
\ Imagine with thyself, courteous reader, how often I then wished for the\n\
\ tongue of Demosthenes or Cicero, that might have enabled me to celebrate\n\
\ the praise of my own dear native country in a style equal to its merits\n\
\ and felicity.\n\
\ \n\
\ I began my discourse by informing his majesty, that our dominions\n\
\ consisted of two islands, which composed three mighty kingdoms, under one\n\
\ sovereign, beside our plantations in America.  I dwelt long upon the\n\
\ fertility of our soil, and the temperature of our climate.  I then spoke\n\
\ at large upon the constitution of an English parliament; partly made up\n\
\ of an illustrious body called the House of Peers; persons of the noblest\n\
\ blood, and of the most ancient and ample patrimonies.  I described that\n\
\ extraordinary care always taken of their education in arts and arms, to\n\
\ qualify them for being counsellors both to the king and kingdom; to have\n\
\ a share in the legislature; to be members of the highest court of\n\
\ judicature, whence there can be no appeal; and to be champions always\n\
\ ready for the defence of their prince and country, by their valour,\n\
\ conduct, and fidelity.  That these were the ornament and bulwark of the\n\
\ kingdom, worthy followers of their most renowned ancestors, whose honour\n\
\ had been the reward of their virtue, from which their posterity were\n\
\ never once known to degenerate.  To these were joined several holy\n\
\ persons, as part of that assembly, under the title of bishops, whose\n\
\ peculiar business is to take care of religion, and of those who instruct\n\
\ the people therein.  These were searched and sought out through the whole\n\
\ nation, by the prince and his wisest counsellors, among such of the\n\
\ priesthood as were most deservedly distinguished by the sanctity of their\n\
\ lives, and the depth of their erudition; who were indeed the spiritual\n\
\ fathers of the clergy and the people.\n\
\ \n\
\ That the other part of the parliament consisted of an assembly called the\n\
\ House of Commons, who were all principal gentlemen, freely picked and\n\
\ culled out by the people themselves, for their great abilities and love\n\
\ of their country, to represent the wisdom of the whole nation.  And that\n\
\ these two bodies made up the most august assembly in Europe; to whom, in\n\
\ conjunction with the prince, the whole legislature is committed.\n\
\ \n\
\ I then descended to the courts of justice; over which the judges, those\n\
\ venerable sages and interpreters of the law, presided, for determining\n\
\ the disputed rights and properties of men, as well as for the punishment\n\
\ of vice and protection of innocence.  I mentioned the prudent management\n\
\ of our treasury; the valour and achievements of our forces, by sea and\n\
\ land.  I computed the number of our people, by reckoning how many\n\
\ millions there might be of each religious sect, or political party among\n\
\ us.  I did not omit even our sports and pastimes, or any other particular\n\
\ which I thought might redound to the honour of my country.  And I\n\
\ finished all with a brief historical account of affairs and events in\n\
\ England for about a hundred years past.\n\
\ \n\
\ This conversation was not ended under five audiences, each of several\n\
\ hours; and the king heard the whole with great attention, frequently\n\
\ taking notes of what I spoke, as well as memorandums of what questions he\n\
\ intended to ask me.\n\
\ \n\
\ When I had put an end to these long discources, his majesty, in a sixth\n\
\ audience, consulting his notes, proposed many doubts, queries, and\n\
\ objections, upon every article.  He asked, \"What methods were used to\n\
\ cultivate the minds and bodies of our young nobility, and in what kind of\n\
\ business they commonly spent the first and teachable parts of their\n\
\ lives?  What course was taken to supply that assembly, when any noble\n\
\ family became extinct?  What qualifications were necessary in those who\n\
\ are to be created new lords: whether the humour of the prince, a sum of\n\
\ money to a court lady, or a design of strengthening a party opposite to\n\
\ the public interest, ever happened to be the motive in those\n\
\ advancements?  What share of knowledge these lords had in the laws of\n\
\ their country, and how they came by it, so as to enable them to decide\n\
\ the properties of their fellow-subjects in the last resort?  Whether they\n\
\ were always so free from avarice, partialities, or want, that a bribe, or\n\
\ some other sinister view, could have no place among them?  Whether those\n\
\ holy lords I spoke of were always promoted to that rank upon account of\n\
\ their knowledge in religious matters, and the sanctity of their lives;\n\
\ had never been compliers with the times, while they were common priests;\n\
\ or slavish prostitute chaplains to some nobleman, whose opinions they\n\
\ continued servilely to follow, after they were admitted into that\n\
\ assembly?\"\n\
\ \n\
\ He then desired to know, \"What arts were practised in electing those whom\n\
\ I called commoners: whether a stranger, with a strong purse, might not\n\
\ influence the vulgar voters to choose him before their own landlord, or\n\
\ the most considerable gentleman in the neighbourhood?  How it came to\n\
\ pass, that people were so violently bent upon getting into this assembly,\n\
\ which I allowed to be a great trouble and expense, often to the ruin of\n\
\ their families, without any salary or pension? because this appeared such\n\
\ an exalted strain of virtue and public spirit, that his majesty seemed to\n\
\ doubt it might possibly not be always sincere.\"  And he desired to know,\n\
\ \"Whether such zealous gentlemen could have any views of refunding\n\
\ themselves for the charges and trouble they were at by sacrificing the\n\
\ public good to the designs of a weak and vicious prince, in conjunction\n\
\ with a corrupted ministry?\"  He multiplied his questions, and sifted me\n\
\ thoroughly upon every part of this head, proposing numberless inquiries\n\
\ and objections, which I think it not prudent or convenient to repeat.\n\
\ \n\
\ Upon what I said in relation to our courts of justice, his majesty\n\
\ desired to be satisfied in several points: and this I was the better able\n\
\ to do, having been formerly almost ruined by a long suit in chancery,\n\
\ which was decreed for me with costs.  He asked, \"What time was usually\n\
\ spent in determining between right and wrong, and what degree of expense?\n\
\ Whether advocates and orators had liberty to plead in causes manifestly\n\
\ known to be unjust, vexatious, or oppressive?  Whether party, in religion\n\
\ or politics, were observed to be of any weight in the scale of justice?\n\
\ Whether those pleading orators were persons educated in the general\n\
\ knowledge of equity, or only in provincial, national, and other local\n\
\ customs?  Whether they or their judges had any part in penning those\n\
\ laws, which they assumed the liberty of interpreting, and glossing upon\n\
\ at their pleasure?  Whether they had ever, at different times, pleaded\n\
\ for and against the same cause, and cited precedents to prove contrary\n\
\ opinions?  Whether they were a rich or a poor corporation?  Whether they\n\
\ received any pecuniary reward for pleading, or delivering their opinions?\n\
\ And particularly, whether they were ever admitted as members in the lower\n\
\ senate?\"\n\
\ \n\
\ He fell next upon the management of our treasury; and said, \"he thought\n\
\ my memory had failed me, because I computed our taxes at about five or\n\
\ six millions a-year, and when I came to mention the issues, he found they\n\
\ sometimes amounted to more than double; for the notes he had taken were\n\
\ very particular in this point, because he hoped, as he told me, that the\n\
\ knowledge of our conduct might be useful to him, and he could not be\n\
\ deceived in his calculations.  But, if what I told him were true, he was\n\
\ still at a loss how a kingdom could run out of its estate, like a private\n\
\ person.\"  He asked me, \"who were our creditors; and where we found money\n\
\ to pay them?\"  He wondered to hear me talk of such chargeable and\n\
\ expensive wars; \"that certainly we must be a quarrelsome people, or live\n\
\ among very bad neighbours, and that our generals must needs be richer\n\
\ than our kings.\"  He asked, what business we had out of our own islands,\n\
\ unless upon the score of trade, or treaty, or to defend the coasts with\n\
\ our fleet?\"  Above all, he was amazed to hear me talk of a mercenary\n\
\ standing army, in the midst of peace, and among a free people.  He said,\n\
\ \"if we were governed by our own consent, in the persons of our\n\
\ representatives, he could not imagine of whom we were afraid, or against\n\
\ whom we were to fight; and would hear my opinion, whether a private man's\n\
\ house might not be better defended by himself, his children, and family,\n\
\ than by half-a-dozen rascals, picked up at a venture in the streets for\n\
\ small wages, who might get a hundred times more by cutting their\n\
\ throats?\"\n\
\ \n\
\ He laughed at my \"odd kind of arithmetic,\" as he was pleased to call it,\n\
\ \"in reckoning the numbers of our people, by a computation drawn from the\n\
\ several sects among us, in religion and politics.\"  He said, \"he knew no\n\
\ reason why those, who entertain opinions prejudicial to the public,\n\
\ should be obliged to change, or should not be obliged to conceal them.\n\
\ And as it was tyranny in any government to require the first, so it was\n\
\ weakness not to enforce the second: for a man may be allowed to keep\n\
\ poisons in his closet, but not to vend them about for cordials.\"\n\
\ \n\
\ He observed, \"that among the diversions of our nobility and gentry, I had\n\
\ mentioned gaming: he desired to know at what age this entertainment was\n\
\ usually taken up, and when it was laid down; how much of their time it\n\
\ employed; whether it ever went so high as to affect their fortunes;\n\
\ whether mean, vicious people, by their dexterity in that art, might not\n\
\ arrive at great riches, and sometimes keep our very nobles in dependence,\n\
\ as well as habituate them to vile companions, wholly take them from the\n\
\ improvement of their minds, and force them, by the losses they received,\n\
\ to learn and practise that infamous dexterity upon others?\"\n\
\ \n\
\ He was perfectly astonished with the historical account gave him of our\n\
\ affairs during the last century; protesting \"it was only a heap of\n\
\ conspiracies, rebellions, murders, massacres, revolutions, banishments,\n\
\ the very worst effects that avarice, faction, hypocrisy, perfidiousness,\n\
\ cruelty, rage, madness, hatred, envy, lust, malice, and ambition, could\n\
\ produce.\"\n\
\ \n\
\ His majesty, in another audience, was at the pains to recapitulate the\n\
\ sum of all I had spoken; compared the questions he made with the answers\n\
\ I had given; then taking me into his hands, and stroking me gently,\n\
\ delivered himself in these words, which I shall never forget, nor the\n\
\ manner he spoke them in: \"My little friend Grildrig, you have made a most\n\
\ admirable panegyric upon your country; you have clearly proved, that\n\
\ ignorance, idleness, and vice, are the proper ingredients for qualifying\n\
\ a legislator; that laws are best explained, interpreted, and applied, by\n\
\ those whose interest and abilities lie in perverting, confounding, and\n\
\ eluding them.  I observe among you some lines of an institution, which,\n\
\ in its original, might have been tolerable, but these half erased, and\n\
\ the rest wholly blurred and blotted by corruptions.  It does not appear,\n\
\ from all you have said, how any one perfection is required toward the\n\
\ procurement of any one station among you; much less, that men are\n\
\ ennobled on account of their virtue; that priests are advanced for their\n\
\ piety or learning; soldiers, for their conduct or valour; judges, for\n\
\ their integrity; senators, for the love of their country; or counsellors\n\
\ for their wisdom.  As for yourself,\" continued the king, \"who have spent\n\
\ the greatest part of your life in travelling, I am well disposed to hope\n\
\ you may hitherto have escaped many vices of your country.  But by what I\n\
\ have gathered from your own relation, and the answers I have with much\n\
\ pains wrung and extorted from you, I cannot but conclude the bulk of your\n\
\ natives to be the most pernicious race of little odious vermin that\n\
\ nature ever suffered to crawl upon the surface of the earth.\"\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER VII.\n\
\ \n\
\ \n\
\ The author's love of his country.  He makes a proposal of much advantage\n\
\ to the king, which is rejected.  The king's great ignorance in politics.\n\
\ The learning of that country very imperfect and confined.  The laws, and\n\
\ military affairs, and parties in the state.\n\
\ \n\
\ Nothing but an extreme love of truth could have hindered me from\n\
\ concealing this part of my story.  It was in vain to discover my\n\
\ resentments, which were always turned into ridicule; and I was forced to\n\
\ rest with patience, while my noble and beloved country was so injuriously\n\
\ treated.  I am as heartily sorry as any of my readers can possibly be,\n\
\ that such an occasion was given: but this prince happened to be so\n\
\ curious and inquisitive upon every particular, that it could not consist\n\
\ either with gratitude or good manners, to refuse giving him what\n\
\ satisfaction I was able.  Yet thus much I may be allowed to say in my own\n\
\ vindication, that I artfully eluded many of his questions, and gave to\n\
\ every point a more favourable turn, by many degrees, than the strictness\n\
\ of truth would allow.  For I have always borne that laudable partiality\n\
\ to my own country, which Dionysius Halicarnassensis, with so much\n\
\ justice, recommends to an historian: I would hide the frailties and\n\
\ deformities of my political mother, and place her virtues and beauties in\n\
\ the most advantageous light.  This was my sincere endeavour in those many\n\
\ discourses I had with that monarch, although it unfortunately failed of\n\
\ success.\n\
\ \n\
\ But great allowances should be given to a king, who lives wholly secluded\n\
\ from the rest of the world, and must therefore be altogether unacquainted\n\
\ with the manners and customs that most prevail in other nations: the want\n\
\ of which knowledge will ever produce many prejudices, and a certain\n\
\ narrowness of thinking, from which we, and the politer countries of\n\
\ Europe, are wholly exempted.  And it would be hard indeed, if so remote a\n\
\ prince's notions of virtue and vice were to be offered as a standard for\n\
\ all mankind.\n\
\ \n\
\ To confirm what I have now said, and further to show the miserable\n\
\ effects of a confined education, I shall here insert a passage, which\n\
\ will hardly obtain belief.  In hopes to ingratiate myself further into\n\
\ his majesty's favour, I told him of \"an invention, discovered between\n\
\ three and four hundred years ago, to make a certain powder, into a heap\n\
\ of which, the smallest spark of fire falling, would kindle the whole in a\n\
\ moment, although it were as big as a mountain, and make it all fly up in\n\
\ the air together, with a noise and agitation greater than thunder.  That\n\
\ a proper quantity of this powder rammed into a hollow tube of brass or\n\
\ iron, according to its bigness, would drive a ball of iron or lead, with\n\
\ such violence and speed, as nothing was able to sustain its force.  That\n\
\ the largest balls thus discharged, would not only destroy whole ranks of\n\
\ an army at once, but batter the strongest walls to the ground, sink down\n\
\ ships, with a thousand men in each, to the bottom of the sea, and when\n\
\ linked together by a chain, would cut through masts and rigging, divide\n\
\ hundreds of bodies in the middle, and lay all waste before them.  That we\n\
\ often put this powder into large hollow balls of iron, and discharged\n\
\ them by an engine into some city we were besieging, which would rip up\n\
\ the pavements, tear the houses to pieces, burst and throw splinters on\n\
\ every side, dashing out the brains of all who came near.  That I knew the\n\
\ ingredients very well, which were cheap and common; I understood the\n\
\ manner of compounding them, and could direct his workmen how to make\n\
\ those tubes, of a size proportionable to all other things in his\n\
\ majesty's kingdom, and the largest need not be above a hundred feet long;\n\
\ twenty or thirty of which tubes, charged with the proper quantity of\n\
\ powder and balls, would batter down the walls of the strongest town in\n\
\ his dominions in a few hours, or destroy the whole metropolis, if ever it\n\
\ should pretend to dispute his absolute commands.\"  This I humbly offered\n\
\ to his majesty, as a small tribute of acknowledgment, in turn for so many\n\
\ marks that I had received, of his royal favour and protection.\n\
\ \n\
\ The king was struck with horror at the description I had given of those\n\
\ terrible engines, and the proposal I had made.  \"He was amazed, how so\n\
\ impotent and grovelling an insect as I\" (these were his expressions)\n\
\ \"could entertain such inhuman ideas, and in so familiar a manner, as to\n\
\ appear wholly unmoved at all the scenes of blood and desolation which I\n\
\ had painted as the common effects of those destructive machines;\n\
\ whereof,\" he said, \"some evil genius, enemy to mankind, must have been\n\
\ the first contriver.  As for himself, he protested, that although few\n\
\ things delighted him so much as new discoveries in art or in nature, yet\n\
\ he would rather lose half his kingdom, than be privy to such a secret;\n\
\ which he commanded me, as I valued any life, never to mention any more.\"\n\
\ \n\
\ A strange effect of narrow principles and views! that a prince possessed\n\
\ of every quality which procures veneration, love, and esteem; of strong\n\
\ parts, great wisdom, and profound learning, endowed with admirable\n\
\ talents, and almost adored by his subjects, should, from a nice,\n\
\ unnecessary scruple, whereof in Europe we can have no conception, let\n\
\ slip an opportunity put into his hands that would have made him absolute\n\
\ master of the lives, the liberties, and the fortunes of his people!\n\
\ Neither do I say this, with the least intention to detract from the many\n\
\ virtues of that excellent king, whose character, I am sensible, will, on\n\
\ this account, be very much lessened in the opinion of an English reader:\n\
\ but I take this defect among them to have risen from their ignorance, by\n\
\ not having hitherto reduced politics into a science, as the more acute\n\
\ wits of Europe have done.  For, I remember very well, in a discourse one\n\
\ day with the king, when I happened to say, \"there were several thousand\n\
\ books among us written upon the art of government,\" it gave him (directly\n\
\ contrary to my intention) a very mean opinion of our understandings.  He\n\
\ professed both to abominate and despise all mystery, refinement, and\n\
\ intrigue, either in a prince or a minister.  He could not tell what I\n\
\ meant by secrets of state, where an enemy, or some rival nation, were not\n\
\ in the case.  He confined the knowledge of governing within very narrow\n\
\ bounds, to common sense and reason, to justice and lenity, to the speedy\n\
\ determination of civil and criminal causes; with some other obvious\n\
\ topics, which are not worth considering.  And he gave it for his opinion,\n\
\ \"that whoever could make two ears of corn, or two blades of grass, to\n\
\ grow upon a spot of ground where only one grew before, would deserve\n\
\ better of mankind, and do more essential service to his country, than the\n\
\ whole race of politicians put together.\"\n\
\ \n\
\ The learning of this people is very defective, consisting only in\n\
\ morality, history, poetry, and mathematics, wherein they must be allowed\n\
\ to excel.  But the last of these is wholly applied to what may be useful\n\
\ in life, to the improvement of agriculture, and all mechanical arts; so\n\
\ that among us, it would be little esteemed.  And as to ideas, entities,\n\
\ abstractions, and transcendentals, I could never drive the least\n\
\ conception into their heads.\n\
\ \n\
\ No law in that country must exceed in words the number of letters in\n\
\ their alphabet, which consists only of two and twenty.  But indeed few of\n\
\ them extend even to that length.  They are expressed in the most plain\n\
\ and simple terms, wherein those people are not mercurial enough to\n\
\ discover above one interpretation: and to write a comment upon any law,\n\
\ is a capital crime.  As to the decision of civil causes, or proceedings\n\
\ against criminals, their precedents are so few, that they have little\n\
\ reason to boast of any extraordinary skill in either.\n\
\ \n\
\ They have had the art of printing, as well as the Chinese, time out of\n\
\ mind: but their libraries are not very large; for that of the king, which\n\
\ is reckoned the largest, does not amount to above a thousand volumes,\n\
\ placed in a gallery of twelve hundred feet long, whence I had liberty to\n\
\ borrow what books I pleased.  The queen's joiner had contrived in one of\n\
\ Glumdalclitch's rooms, a kind of wooden machine five-and-twenty feet\n\
\ high, formed like a standing ladder; the steps were each fifty feet long.\n\
\ It was indeed a moveable pair of stairs, the lowest end placed at ten\n\
\ feet distance from the wall of the chamber.  The book I had a mind to\n\
\ read, was put up leaning against the wall: I first mounted to the upper\n\
\ step of the ladder, and turning my face towards the book, began at the\n\
\ top of the page, and so walking to the right and left about eight or ten\n\
\ paces, according to the length of the lines, till I had gotten a little\n\
\ below the level of mine eyes, and then descending gradually till I came\n\
\ to the bottom: after which I mounted again, and began the other page in\n\
\ the same manner, and so turned over the leaf, which I could easily do\n\
\ with both my hands, for it was as thick and stiff as a pasteboard, and in\n\
\ the largest folios not above eighteen or twenty feet long.\n\
\ \n\
\ Their style is clear, masculine, and smooth, but not florid; for they\n\
\ avoid nothing more than multiplying unnecessary words, or using various\n\
\ expressions.  I have perused many of their books, especially those in\n\
\ history and morality.  Among the rest, I was much diverted with a little\n\
\ old treatise, which always lay in Glumdalclitch's bed chamber, and\n\
\ belonged to her governess, a grave elderly gentlewoman, who dealt in\n\
\ writings of morality and devotion.  The book treats of the weakness of\n\
\ human kind, and is in little esteem, except among the women and the\n\
\ vulgar.  However, I was curious to see what an author of that country\n\
\ could say upon such a subject.  This writer went through all the usual\n\
\ topics of European moralists, showing \"how diminutive, contemptible, and\n\
\ helpless an animal was man in his own nature; how unable to defend\n\
\ himself from inclemencies of the air, or the fury of wild beasts: how\n\
\ much he was excelled by one creature in strength, by another in speed, by\n\
\ a third in foresight, by a fourth in industry.\"  He added, \"that nature\n\
\ was degenerated in these latter declining ages of the world, and could\n\
\ now produce only small abortive births, in comparison of those in ancient\n\
\ times.\"  He said \"it was very reasonable to think, not only that the\n\
\ species of men were originally much larger, but also that there must have\n\
\ been giants in former ages; which, as it is asserted by history and\n\
\ tradition, so it has been confirmed by huge bones and skulls, casually\n\
\ dug up in several parts of the kingdom, far exceeding the common dwindled\n\
\ race of men in our days.\"  He argued, \"that the very laws of nature\n\
\ absolutely required we should have been made, in the beginning of a size\n\
\ more large and robust; not so liable to destruction from every little\n\
\ accident, of a tile falling from a house, or a stone cast from the hand\n\
\ of a boy, or being drowned in a little brook.\"  From this way of\n\
\ reasoning, the author drew several moral applications, useful in the\n\
\ conduct of life, but needless here to repeat.  For my own part, I could\n\
\ not avoid reflecting how universally this talent was spread, of drawing\n\
\ lectures in morality, or indeed rather matter of discontent and repining,\n\
\ from the quarrels we raise with nature.  And I believe, upon a strict\n\
\ inquiry, those quarrels might be shown as ill-grounded among us as they\n\
\ are among that people.\n\
\ \n\
\ As to their military affairs, they boast that the king's army consists of\n\
\ a hundred and seventy-six thousand foot, and thirty-two thousand horse:\n\
\ if that may be called an army, which is made up of tradesmen in the\n\
\ several cities, and farmers in the country, whose commanders are only the\n\
\ nobility and gentry, without pay or reward.  They are indeed perfect\n\
\ enough in their exercises, and under very good discipline, wherein I saw\n\
\ no great merit; for how should it be otherwise, where every farmer is\n\
\ under the command of his own landlord, and every citizen under that of\n\
\ the principal men in his own city, chosen after the manner of Venice, by\n\
\ ballot?\n\
\ \n\
\ I have often seen the militia of Lorbrulgrud drawn out to exercise, in a\n\
\ great field near the city of twenty miles square.  They were in all not\n\
\ above twenty-five thousand foot, and six thousand horse; but it was\n\
\ impossible for me to compute their number, considering the space of\n\
\ ground they took up.  A cavalier, mounted on a large steed, might be\n\
\ about ninety feet high.  I have seen this whole body of horse, upon a\n\
\ word of command, draw their swords at once, and brandish them in the air.\n\
\ Imagination can figure nothing so grand, so surprising, and so\n\
\ astonishing! it looked as if ten thousand flashes of lightning were\n\
\ darting at the same time from every quarter of the sky.\n\
\ \n\
\ I was curious to know how this prince, to whose dominions there is no\n\
\ access from any other country, came to think of armies, or to teach his\n\
\ people the practice of military discipline.  But I was soon informed,\n\
\ both by conversation and reading their histories; for, in the course of\n\
\ many ages, they have been troubled with the same disease to which the\n\
\ whole race of mankind is subject; the nobility often contending for\n\
\ power, the people for liberty, and the king for absolute dominion.  All\n\
\ which, however happily tempered by the laws of that kingdom, have been\n\
\ sometimes violated by each of the three parties, and have more than once\n\
\ occasioned civil wars; the last whereof was happily put an end to by this\n\
\ prince's grand-father, in a general composition; and the militia, then\n\
\ settled with common consent, has been ever since kept in the strictest\n\
\ duty.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER VIII.\n\
\ \n\
\ \n\
\ The king and queen make a progress to the frontiers.  The author attends\n\
\ them.  The manner in which he leaves the country very particularly\n\
\ related.  He returns to England.\n\
\ \n\
\ I had always a strong impulse that I should some time recover my liberty,\n\
\ though it was impossible to conjecture by what means, or to form any\n\
\ project with the least hope of succeeding.  The ship in which I sailed,\n\
\ was the first ever known to be driven within sight of that coast, and the\n\
\ king had given strict orders, that if at any time another appeared, it\n\
\ should be taken ashore, and with all its crew and passengers brought in a\n\
\ tumbril to Lorbrulgrud.  He was strongly bent to get me a woman of my own\n\
\ size, by whom I might propagate the breed: but I think I should rather\n\
\ have died than undergone the disgrace of leaving a posterity to be kept\n\
\ in cages, like tame canary-birds, and perhaps, in time, sold about the\n\
\ kingdom, to persons of quality, for curiosities.  I was indeed treated\n\
\ with much kindness: I was the favourite of a great king and queen, and\n\
\ the delight of the whole court; but it was upon such a foot as ill became\n\
\ the dignity of humankind.  I could never forget those domestic pledges I\n\
\ had left behind me.  I wanted to be among people, with whom I could\n\
\ converse upon even terms, and walk about the streets and fields without\n\
\ being afraid of being trod to death like a frog or a young puppy.  But my\n\
\ deliverance came sooner than I expected, and in a manner not very common;\n\
\ the whole story and circumstances of which I shall faithfully relate.\n\
\ \n\
\ I had now been two years in this country; and about the beginning of the\n\
\ third, Glumdalclitch and I attended the king and queen, in a progress to\n\
\ the south coast of the kingdom.  I was carried, as usual, in my\n\
\ travelling-box, which as I have already described, was a very convenient\n\
\ closet, of twelve feet wide.  And I had ordered a hammock to be fixed, by\n\
\ silken ropes from the four corners at the top, to break the jolts, when a\n\
\ servant carried me before him on horseback, as I sometimes desired; and\n\
\ would often sleep in my hammock, while we were upon the road.  On the\n\
\ roof of my closet, not directly over the middle of the hammock, I ordered\n\
\ the joiner to cut out a hole of a foot square, to give me air in hot\n\
\ weather, as I slept; which hole I shut at pleasure with a board that drew\n\
\ backward and forward through a groove.\n\
\ \n\
\ When we came to our journey's end, the king thought proper to pass a few\n\
\ days at a palace he has near Flanflasnic, a city within eighteen English\n\
\ miles of the seaside.  Glumdalclitch and I were much fatigued: I had\n\
\ gotten a small cold, but the poor girl was so ill as to be confined to\n\
\ her chamber.  I longed to see the ocean, which must be the only scene of\n\
\ my escape, if ever it should happen.  I pretended to be worse than I\n\
\ really was, and desired leave to take the fresh air of the sea, with a\n\
\ page, whom I was very fond of, and who had sometimes been trusted with\n\
\ me.  I shall never forget with what unwillingness Glumdalclitch\n\
\ consented, nor the strict charge she gave the page to be careful of me,\n\
\ bursting at the same time into a flood of tears, as if she had some\n\
\ forboding of what was to happen.  The boy took me out in my box, about\n\
\ half an hours walk from the palace, towards the rocks on the sea-shore.\n\
\ I ordered him to set me down, and lifting up one of my sashes, cast many\n\
\ a wistful melancholy look towards the sea.  I found myself not very well,\n\
\ and told the page that I had a mind to take a nap in my hammock, which I\n\
\ hoped would do me good.  I got in, and the boy shut the window close\n\
\ down, to keep out the cold.  I soon fell asleep, and all I can conjecture\n\
\ is, while I slept, the page, thinking no danger could happen, went among\n\
\ the rocks to look for birds' eggs, having before observed him from my\n\
\ window searching about, and picking up one or two in the clefts.  Be that\n\
\ as it will, I found myself suddenly awaked with a violent pull upon the\n\
\ ring, which was fastened at the top of my box for the conveniency of\n\
\ carriage.  I felt my box raised very high in the air, and then borne\n\
\ forward with prodigious speed.  The first jolt had like to have shaken me\n\
\ out of my hammock, but afterward the motion was easy enough.  I called\n\
\ out several times, as loud as I could raise my voice, but all to no\n\
\ purpose.  I looked towards my windows, and could see nothing but the\n\
\ clouds and sky.  I heard a noise just over my head, like the clapping of\n\
\ wings, and then began to perceive the woful condition I was in; that some\n\
\ eagle had got the ring of my box in his beak, with an intent to let it\n\
\ fall on a rock, like a tortoise in a shell, and then pick out my body,\n\
\ and devour it: for the sagacity and smell of this bird enables him to\n\
\ discover his quarry at a great distance, though better concealed than I\n\
\ could be within a two-inch board.\n\
\ \n\
\ In a little time, I observed the noise and flutter of wings to increase\n\
\ very fast, and my box was tossed up and down, like a sign in a windy day.\n\
\ I heard several bangs or buffets, as I thought given to the eagle (for\n\
\ such I am certain it must have been that held the ring of my box in his\n\
\ beak), and then, all on a sudden, felt myself falling perpendicularly\n\
\ down, for above a minute, but with such incredible swiftness, that I\n\
\ almost lost my breath.  My fall was stopped by a terrible squash, that\n\
\ sounded louder to my ears than the cataract of Niagara; after which, I\n\
\ was quite in the dark for another minute, and then my box began to rise\n\
\ so high, that I could see light from the tops of the windows.  I now\n\
\ perceived I was fallen into the sea.  My box, by the weight of my body,\n\
\ the goods that were in, and the broad plates of iron fixed for strength\n\
\ at the four corners of the top and bottom, floated about five feet deep\n\
\ in water.  I did then, and do now suppose, that the eagle which flew away\n\
\ with my box was pursued by two or three others, and forced to let me\n\
\ drop, while he defended himself against the rest, who hoped to share in\n\
\ the prey.  The plates of iron fastened at the bottom of the box (for\n\
\ those were the strongest) preserved the balance while it fell, and\n\
\ hindered it from being broken on the surface of the water. Every joint of\n\
\ it was well grooved; and the door did not move on hinges, but up and down\n\
\ like a sash, which kept my closet so tight that very little water came\n\
\ in.  I got with much difficulty out of my hammock, having first ventured\n\
\ to draw back the slip-board on the roof already mentioned, contrived on\n\
\ purpose to let in air, for want of which I found myself almost stifled.\n\
\ \n\
\ How often did I then wish myself with my dear Glumdalclitch, from whom\n\
\ one single hour had so far divided me!  And I may say with truth, that in\n\
\ the midst of my own misfortunes I could not forbear lamenting my poor\n\
\ nurse, the grief she would suffer for my loss, the displeasure of the\n\
\ queen, and the ruin of her fortune.  Perhaps many travellers have not\n\
\ been under greater difficulties and distress than I was at this juncture,\n\
\ expecting every moment to see my box dashed to pieces, or at least\n\
\ overset by the first violent blast, or rising wave.  A breach in one\n\
\ single pane of glass would have been immediate death: nor could any thing\n\
\ have preserved the windows, but the strong lattice wires placed on the\n\
\ outside, against accidents in travelling.  I saw the water ooze in at\n\
\ several crannies, although the leaks were not considerable, and I\n\
\ endeavoured to stop them as well as I could.  I was not able to lift up\n\
\ the roof of my closet, which otherwise I certainly should have done, and\n\
\ sat on the top of it; where I might at least preserve myself some hours\n\
\ longer, than by being shut up (as I may call it) in the hold.  Or if I\n\
\ escaped these dangers for a day or two, what could I expect but a\n\
\ miserable death of cold and hunger?  I was four hours under these\n\
\ circumstances, expecting, and indeed wishing, every moment to be my last.\n\
\ \n\
\ I have already told the reader that there were two strong staples fixed\n\
\ upon that side of my box which had no window, and into which the servant,\n\
\ who used to carry me on horseback, would put a leathern belt, and buckle\n\
\ it about his waist.  Being in this disconsolate state, I heard, or at\n\
\ least thought I heard, some kind of grating noise on that side of my box\n\
\ where the staples were fixed; and soon after I began to fancy that the\n\
\ box was pulled or towed along the sea; for I now and then felt a sort of\n\
\ tugging, which made the waves rise near the tops of my windows, leaving\n\
\ me almost in the dark.  This gave me some faint hopes of relief, although\n\
\ I was not able to imagine how it could be brought about.  I ventured to\n\
\ unscrew one of my chairs, which were always fastened to the floor; and\n\
\ having made a hard shift to screw it down again, directly under the\n\
\ slipping-board that I had lately opened, I mounted on the chair, and\n\
\ putting my mouth as near as I could to the hole, I called for help in a\n\
\ loud voice, and in all the languages I understood.  I then fastened my\n\
\ handkerchief to a stick I usually carried, and thrusting it up the hole,\n\
\ waved it several times in the air, that if any boat or ship were near,\n\
\ the seamen might conjecture some unhappy mortal to be shut up in the box.\n\
\ \n\
\ I found no effect from all I could do, but plainly perceived my closet to\n\
\ be moved along; and in the space of an hour, or better, that side of the\n\
\ box where the staples were, and had no windows, struck against something\n\
\ that was hard.  I apprehended it to be a rock, and found myself tossed\n\
\ more than ever.  I plainly heard a noise upon the cover of my closet,\n\
\ like that of a cable, and the grating of it as it passed through the\n\
\ ring.  I then found myself hoisted up, by degrees, at least three feet\n\
\ higher than I was before.  Whereupon I again thrust up my stick and\n\
\ handkerchief, calling for help till I was almost hoarse.  In return to\n\
\ which, I heard a great shout repeated three times, giving me such\n\
\ transports of joy as are not to be conceived but by those who feel them.\n\
\ I now heard a trampling over my head, and somebody calling through the\n\
\ hole with a loud voice, in the English tongue, \"If there be any body\n\
\ below, let them speak.\"  I answered, \"I was an Englishman, drawn by ill\n\
\ fortune into the greatest calamity that ever any creature underwent, and\n\
\ begged, by all that was moving, to be delivered out of the dungeon I was\n\
\ in.\"  The voice replied, \"I was safe, for my box was fastened to their\n\
\ ship; and the carpenter should immediately come and saw a hole in the\n\
\ cover, large enough to pull me out.\"  I answered, \"that was needless, and\n\
\ would take up too much time; for there was no more to be done, but let\n\
\ one of the crew put his finger into the ring, and take the box out of the\n\
\ sea into the ship, and so into the captain's cabin.\"  Some of them, upon\n\
\ hearing me talk so wildly, thought I was mad: others laughed; for indeed\n\
\ it never came into my head, that I was now got among people of my own\n\
\ stature and strength.  The carpenter came, and in a few minutes sawed a\n\
\ passage about four feet square, then let down a small ladder, upon which\n\
\ I mounted, and thence was taken into the ship in a very weak condition.\n\
\ \n\
\ The sailors were all in amazement, and asked me a thousand questions,\n\
\ which I had no inclination to answer.  I was equally confounded at the\n\
\ sight of so many pigmies, for such I took them to be, after having so\n\
\ long accustomed mine eyes to the monstrous objects I had left.  But the\n\
\ captain, Mr. Thomas Wilcocks, an honest worthy Shropshire man, observing\n\
\ I was ready to faint, took me into his cabin, gave me a cordial to\n\
\ comfort me, and made me turn in upon his own bed, advising me to take a\n\
\ little rest, of which I had great need.  Before I went to sleep, I gave\n\
\ him to understand that I had some valuable furniture in my box, too good\n\
\ to be lost: a fine hammock, a handsome field-bed, two chairs, a table,\n\
\ and a cabinet; that my closet was hung on all sides, or rather quilted,\n\
\ with silk and cotton; that if he would let one of the crew bring my\n\
\ closet into his cabin, I would open it there before him, and show him my\n\
\ goods.  The captain, hearing me utter these absurdities, concluded I was\n\
\ raving; however (I suppose to pacify me) he promised to give order as I\n\
\ desired, and going upon deck, sent some of his men down into my closet,\n\
\ whence (as I afterwards found) they drew up all my goods, and stripped\n\
\ off the quilting; but the chairs, cabinet, and bedstead, being screwed to\n\
\ the floor, were much damaged by the ignorance of the seamen, who tore\n\
\ them up by force.  Then they knocked off some of the boards for the use\n\
\ of the ship, and when they had got all they had a mind for, let the hull\n\
\ drop into the sea, which by reason of many breaches made in the bottom\n\
\ and sides, sunk to rights.  And, indeed, I was glad not to have been a\n\
\ spectator of the havoc they made, because I am confident it would have\n\
\ sensibly touched me, by bringing former passages into my mind, which I\n\
\ would rather have forgot.\n\
\ \n\
\ I slept some hours, but perpetually disturbed with dreams of the place I\n\
\ had left, and the dangers I had escaped.  However, upon waking, I found\n\
\ myself much recovered.  It was now about eight o'clock at night, and the\n\
\ captain ordered supper immediately, thinking I had already fasted too\n\
\ long.  He entertained me with great kindness, observing me not to look\n\
\ wildly, or talk inconsistently: and, when we were left alone, desired I\n\
\ would give him a relation of my travels, and by what accident I came to\n\
\ be set adrift, in that monstrous wooden chest.  He said \"that about\n\
\ twelve o'clock at noon, as he was looking through his glass, he spied it\n\
\ at a distance, and thought it was a sail, which he had a mind to make,\n\
\ being not much out of his course, in hopes of buying some biscuit, his\n\
\ own beginning to fall short.  That upon coming nearer, and finding his\n\
\ error, he sent out his long-boat to discover what it was; that his men\n\
\ came back in a fright, swearing they had seen a swimming house.  That he\n\
\ laughed at their folly, and went himself in the boat, ordering his men to\n\
\ take a strong cable along with them.  That the weather being calm, he\n\
\ rowed round me several times, observed my windows and wire lattices that\n\
\ defended them.  That he discovered two staples upon one side, which was\n\
\ all of boards, without any passage for light.  He then commanded his men\n\
\ to row up to that side, and fastening a cable to one of the staples,\n\
\ ordered them to tow my chest, as they called it, toward the ship.  When\n\
\ it was there, he gave directions to fasten another cable to the ring\n\
\ fixed in the cover, and to raise up my chest with pulleys, which all the\n\
\ sailors were not able to do above two or three feet.\"  He said, \"they saw\n\
\ my stick and handkerchief thrust out of the hole, and concluded that some\n\
\ unhappy man must be shut up in the cavity.\"  I asked, \"whether he or the\n\
\ crew had seen any prodigious birds in the air, about the time he first\n\
\ discovered me.\"  To which he answered, \"that discoursing this matter with\n\
\ the sailors while I was asleep, one of them said, he had observed three\n\
\ eagles flying towards the north, but remarked nothing of their being\n\
\ larger than the usual size:\" which I suppose must be imputed to the great\n\
\ height they were at; and he could not guess the reason of my question.  I\n\
\ then asked the captain, \"how far he reckoned we might be from land?\"  He\n\
\ said, \"by the best computation he could make, we were at least a hundred\n\
\ leagues.\"  I assured him, \"that he must be mistaken by almost half, for I\n\
\ had not left the country whence I came above two hours before I dropped\n\
\ into the sea.\"  Whereupon he began again to think that my brain was\n\
\ disturbed, of which he gave me a hint, and advised me to go to bed in a\n\
\ cabin he had provided.  I assured him, \"I was well refreshed with his\n\
\ good entertainment and company, and as much in my senses as ever I was in\n\
\ my life.\"  He then grew serious, and desired to ask me freely, \"whether I\n\
\ were not troubled in my mind by the consciousness of some enormous crime,\n\
\ for which I was punished, at the command of some prince, by exposing me\n\
\ in that chest; as great criminals, in other countries, have been forced\n\
\ to sea in a leaky vessel, without provisions: for although he should be\n\
\ sorry to have taken so ill a man into his ship, yet he would engage his\n\
\ word to set me safe ashore, in the first port where we arrived.\"  He\n\
\ added, \"that his suspicions were much increased by some very absurd\n\
\ speeches I had delivered at first to his sailors, and afterwards to\n\
\ himself, in relation to my closet or chest, as well as by my odd looks\n\
\ and behaviour while I was at supper.\"\n\
\ \n\
\ I begged his patience to hear me tell my story, which I faithfully did,\n\
\ from the last time I left England, to the moment he first discovered me.\n\
\ And, as truth always forces its way into rational minds, so this honest\n\
\ worthy gentleman, who had some tincture of learning, and very good sense,\n\
\ was immediately convinced of my candour and veracity.  But further to\n\
\ confirm all I had said, I entreated him to give order that my cabinet\n\
\ should be brought, of which I had the key in my pocket; for he had\n\
\ already informed me how the seamen disposed of my closet.  I opened it in\n\
\ his own presence, and showed him the small collection of rarities I made\n\
\ in the country from which I had been so strangely delivered.  There was\n\
\ the comb I had contrived out of the stumps of the king's beard, and\n\
\ another of the same materials, but fixed into a paring of her majesty's\n\
\ thumb-nail, which served for the back.  There was a collection of needles\n\
\ and pins, from a foot to half a yard long; four wasp stings, like\n\
\ joiner's tacks; some combings of the queen's hair; a gold ring, which one\n\
\ day she made me a present of, in a most obliging manner, taking it from\n\
\ her little finger, and throwing it over my head like a collar.  I desired\n\
\ the captain would please to accept this ring in return for his\n\
\ civilities; which he absolutely refused.  I showed him a corn that I had\n\
\ cut off with my own hand, from a maid of honour's toe; it was about the\n\
\ bigness of Kentish pippin, and grown so hard, that when I returned\n\
\ England, I got it hollowed into a cup, and set in silver.  Lastly, I\n\
\ desired him to see the breeches I had then on, which were made of a\n\
\ mouse's skin.\n\
\ \n\
\ I could force nothing on him but a footman's tooth, which I observed him\n\
\ to examine with great curiosity, and found he had a fancy for it.  He\n\
\ received it with abundance of thanks, more than such a trifle could\n\
\ deserve.  It was drawn by an unskilful surgeon, in a mistake, from one of\n\
\ Glumdalclitch's men, who was afflicted with the tooth-ache, but it was as\n\
\ sound as any in his head.  I got it cleaned, and put it into my cabinet.\n\
\ It was about a foot long, and four inches in diameter.\n\
\ \n\
\ The captain was very well satisfied with this plain relation I had given\n\
\ him, and said, \"he hoped, when we returned to England, I would oblige the\n\
\ world by putting it on paper, and making it public.\"  My answer was,\n\
\ \"that we were overstocked with books of travels: that nothing could now\n\
\ pass which was not extraordinary; wherein I doubted some authors less\n\
\ consulted truth, than their own vanity, or interest, or the diversion of\n\
\ ignorant readers; that my story could contain little beside common\n\
\ events, without those ornamental descriptions of strange plants, trees,\n\
\ birds, and other animals; or of the barbarous customs and idolatry of\n\
\ savage people, with which most writers abound.  However, I thanked him\n\
\ for his good opinion, and promised to take the matter into my thoughts.\"\n\
\ \n\
\ He said \"he wondered at one thing very much, which was, to hear me speak\n\
\ so loud;\" asking me \"whether the king or queen of that country were thick\n\
\ of hearing?\"  I told him, \"it was what I had been used to for above two\n\
\ years past, and that I admired as much at the voices of him and his men,\n\
\ who seemed to me only to whisper, and yet I could hear them well enough.\n\
\ But, when I spoke in that country, it was like a man talking in the\n\
\ streets, to another looking out from the top of a steeple, unless when I\n\
\ was placed on a table, or held in any person's hand.\"  I told him, \"I had\n\
\ likewise observed another thing, that, when I first got into the ship,\n\
\ and the sailors stood all about me, I thought they were the most little\n\
\ contemptible creatures I had ever beheld.\"  For indeed, while I was in\n\
\ that prince's country, I could never endure to look in a glass, after\n\
\ mine eyes had been accustomed to such prodigious objects, because the\n\
\ comparison gave me so despicable a conceit of myself.  The captain said,\n\
\ \"that while we were at supper, he observed me to look at every thing with\n\
\ a sort of wonder, and that I often seemed hardly able to contain my\n\
\ laughter, which he knew not well how to take, but imputed it to some\n\
\ disorder in my brain.\"  I answered, \"it was very true; and I wondered how\n\
\ I could forbear, when I saw his dishes of the size of a silver\n\
\ three-pence, a leg of pork hardly a mouthful, a cup not so big as a\n\
\ nut-shell;\" and so I went on, describing the rest of his household-stuff\n\
\ and provisions, after the same manner.  For, although he queen had\n\
\ ordered a little equipage of all things necessary for me, while I was in\n\
\ her service, yet my ideas were wholly taken up with what I saw on every\n\
\ side of me, and I winked at my own littleness, as people do at their own\n\
\ faults.  The captain understood my raillery very well, and merrily\n\
\ replied with the old English proverb, \"that he doubted mine eyes were\n\
\ bigger than my belly, for he did not observe my stomach so good, although\n\
\ I had fasted all day;\" and, continuing in his mirth, protested \"he would\n\
\ have gladly given a hundred pounds, to have seen my closet in the eagle's\n\
\ bill, and afterwards in its fall from so great a height into the sea;\n\
\ which would certainly have been a most astonishing object, worthy to have\n\
\ the description of it transmitted to future ages:\" and the comparison of\n\
\ Phaeton was so obvious, that he could not forbear applying it, although I\n\
\ did not much admire the conceit.\n\
\ \n\
\ The captain having been at Tonquin, was, in his return to England, driven\n\
\ north-eastward to the latitude of 44 degrees, and longitude of 143.  But\n\
\ meeting a trade-wind two days after I came on board him, we sailed\n\
\ southward a long time, and coasting New Holland, kept our course\n\
\ west-south-west, and then south-south-west, till we doubled the Cape of\n\
\ Good Hope.  Our voyage was very prosperous, but I shall not trouble the\n\
\ reader with a journal of it.  The captain called in at one or two ports,\n\
\ and sent in his long-boat for provisions and fresh water; but I never\n\
\ went out of the ship till we came into the Downs, which was on the third\n\
\ day of June, 1706, about nine months after my escape.  I offered to leave\n\
\ my goods in security for payment of my freight: but the captain protested\n\
\ he would not receive one farthing.  We took a kind leave of each other,\n\
\ and I made him promise he would come to see me at my house in Redriff.  I\n\
\ hired a horse and guide for five shillings, which I borrowed of the\n\
\ captain.\n\
\ \n\
\ As I was on the road, observing the littleness of the houses, the trees,\n\
\ the cattle, and the people, I began to think myself in Lilliput.  I was\n\
\ afraid of trampling on every traveller I met, and often called aloud to\n\
\ have them stand out of the way, so that I had like to have gotten one or\n\
\ two broken heads for my impertinence.\n\
\ \n\
\ When I came to my own house, for which I was forced to inquire, one of\n\
\ the servants opening the door, I bent down to go in, (like a goose under\n\
\ a gate,) for fear of striking my head.  My wife run out to embrace me,\n\
\ but I stooped lower than her knees, thinking she could otherwise never be\n\
\ able to reach my mouth.  My daughter kneeled to ask my blessing, but I\n\
\ could not see her till she arose, having been so long used to stand with\n\
\ my head and eyes erect to above sixty feet; and then I went to take her\n\
\ up with one hand by the waist.  I looked down upon the servants, and one\n\
\ or two friends who were in the house, as if they had been pigmies and I a\n\
\ giant.  I told my wife, \"she had been too thrifty, for I found she had\n\
\ starved herself and her daughter to nothing.\"  In short, I behaved myself\n\
\ so unaccountably, that they were all of the captain's opinion when he\n\
\ first saw me, and concluded I had lost my wits.  This I mention as an\n\
\ instance of the great power of habit and prejudice.\n\
\ \n\
\ In a little time, I and my family and friends came to a right\n\
\ understanding: but my wife protested \"I should never go to sea any more;\"\n\
\ although my evil destiny so ordered, that she had not power to hinder me,\n\
\ as the reader may know hereafter.  In the mean time, I here conclude the\n\
\ second part of my unfortunate voyages.\n\
\ \n\
\ \n\
\ \n\
\ \n\
\ \n\
\ \n\
\ PART III.  A VOYAGE TO LAPUTA, BALNIBARBI, LUGGNAGG, GLUBBDUBDRIB, AND\n\
\ JAPAN.\n\
\ \n\
\ \n\
\ \n\
\ \n\
\ \n\
\ \n\
\ CHAPTER I.\n\
\ \n\
\ \n\
\ The author sets out on his third voyage.  Is taken by pirates.  The\n\
\ malice of a Dutchman.  His arrival at an island.  He is received into\n\
\ Laputa.\n\
\ \n\
\ I had not been at home above ten days, when Captain William Robinson, a\n\
\ Cornish man, commander of the Hopewell, a stout ship of three hundred\n\
\ tons, came to my house.  I had formerly been surgeon of another ship\n\
\ where he was master, and a fourth part owner, in a voyage to the Levant.\n\
\ He had always treated me more like a brother, than an inferior officer;\n\
\ and, hearing of my arrival, made me a visit, as I apprehended only out of\n\
\ friendship, for nothing passed more than what is usual after long\n\
\ absences.  But repeating his visits often, expressing his joy to find I\n\
\ me in good health, asking, \"whether I were now settled for life?\" adding,\n\
\ \"that he intended a voyage to the East Indies in two months,\" at last he\n\
\ plainly invited me, though with some apologies, to be surgeon of the\n\
\ ship; \"that I should have another surgeon under me, beside our two mates;\n\
\ that my salary should be double to the usual pay; and that having\n\
\ experienced my knowledge in sea-affairs to be at least equal to his, he\n\
\ would enter into any engagement to follow my advice, as much as if I had\n\
\ shared in the command.\"\n\
\ \n\
\ He said so many other obliging things, and I knew him to be so honest a\n\
\ man, that I could not reject this proposal; the thirst I had of seeing\n\
\ the world, notwithstanding my past misfortunes, continuing as violent as\n\
\ ever.  The only difficulty that remained, was to persuade my wife, whose\n\
\ consent however I at last obtained, by the prospect of advantage she\n\
\ proposed to her children.\n\
\ \n\
\ We set out the 5th day of August, 1706, and arrived at Fort St. George\n\
\ the 11th of April, 1707.  We staid there three weeks to refresh our crew,\n\
\ many of whom were sick.  From thence we went to Tonquin, where the\n\
\ captain resolved to continue some time, because many of the goods he\n\
\ intended to buy were not ready, nor could he expect to be dispatched in\n\
\ several months.  Therefore, in hopes to defray some of the charges he\n\
\ must be at, he bought a sloop, loaded it with several sorts of goods,\n\
\ wherewith the Tonquinese usually trade to the neighbouring islands, and\n\
\ putting fourteen men on board, whereof three were of the country, he\n\
\ appointed me master of the sloop, and gave me power to traffic, while he\n\
\ transacted his affairs at Tonquin.\n\
\ \n\
\ We had not sailed above three days, when a great storm arising, we were\n\
\ driven five days to the north-north-east, and then to the east: after\n\
\ which we had fair weather, but still with a pretty strong gale from the\n\
\ west.  Upon the tenth day we were chased by two pirates, who soon\n\
\ overtook us; for my sloop was so deep laden, that she sailed very slow,\n\
\ neither were we in a condition to defend ourselves.\n\
\ \n\
\ We were boarded about the same time by both the pirates, who entered\n\
\ furiously at the head of their men; but finding us all prostrate upon our\n\
\ faces (for so I gave order), they pinioned us with strong ropes, and\n\
\ setting guard upon us, went to search the sloop.\n\
\ \n\
\ I observed among them a Dutchman, who seemed to be of some authority,\n\
\ though he was not commander of either ship.  He knew us by our\n\
\ countenances to be Englishmen, and jabbering to us in his own language,\n\
\ swore we should be tied back to back and thrown into the sea.  I spoken\n\
\ Dutch tolerably well; I told him who we were, and begged him, in\n\
\ consideration of our being Christians and Protestants, of neighbouring\n\
\ countries in strict alliance, that he would move the captains to take\n\
\ some pity on us.  This inflamed his rage; he repeated his threatenings,\n\
\ and turning to his companions, spoke with great vehemence in the Japanese\n\
\ language, as I suppose, often using the word _Christianos_.\n\
\ \n\
\ The largest of the two pirate ships was commanded by a Japanese captain,\n\
\ who spoke a little Dutch, but very imperfectly.  He came up to me, and\n\
\ after several questions, which I answered in great humility, he said, \"we\n\
\ should not die.\"  I made the captain a very low bow, and then, turning to\n\
\ the Dutchman, said, \"I was sorry to find more mercy in a heathen, than in\n\
\ a brother christian.\"  But I had soon reason to repent those foolish\n\
\ words: for that malicious reprobate, having often endeavoured in vain to\n\
\ persuade both the captains that I might be thrown into the sea (which\n\
\ they would not yield to, after the promise made me that I should not\n\
\ die), however, prevailed so far, as to have a punishment inflicted on me,\n\
\ worse, in all human appearance, than death itself.  My men were sent by\n\
\ an equal division into both the pirate ships, and my sloop new manned.\n\
\ As to myself, it was determined that I should be set adrift in a small\n\
\ canoe, with paddles and a sail, and four days' provisions; which last,\n\
\ the Japanese captain was so kind to double out of his own stores, and\n\
\ would permit no man to search me.  I got down into the canoe, while the\n\
\ Dutchman, standing upon the deck, loaded me with all the curses and\n\
\ injurious terms his language could afford.\n\
\ \n\
\ About an hour before we saw the pirates I had taken an observation, and\n\
\ found we were in the latitude of 46 N. and longitude of 183.  When I was\n\
\ at some distance from the pirates, I discovered, by my pocket-glass,\n\
\ several islands to the south-east.  I set up my sail, the wind being\n\
\ fair, with a design to reach the nearest of those islands, which I made a\n\
\ shift to do, in about three hours.  It was all rocky: however I got many\n\
\ birds' eggs; and, striking fire, I kindled some heath and dry sea-weed,\n\
\ by which I roasted my eggs.  I ate no other supper, being resolved to\n\
\ spare my provisions as much as I could.  I passed the night under the\n\
\ shelter of a rock, strewing some heath under me, and slept pretty well.\n\
\ \n\
\ The next day I sailed to another island, and thence to a third and\n\
\ fourth, sometimes using my sail, and sometimes my paddles.  But, not to\n\
\ trouble the reader with a particular account of my distresses, let it\n\
\ suffice, that on the fifth day I arrived at the last island in my sight,\n\
\ which lay south-south-east to the former.\n\
\ \n\
\ This island was at a greater distance than I expected, and I did not\n\
\ reach it in less than five hours.  I encompassed it almost round, before\n\
\ I could find a convenient place to land in; which was a small creek,\n\
\ about three times the wideness of my canoe.  I found the island to be all\n\
\ rocky, only a little intermingled with tufts of grass, and sweet-smelling\n\
\ herbs.  I took out my small provisions and after having refreshed myself,\n\
\ I secured the remainder in a cave, whereof there were great numbers; I\n\
\ gathered plenty of eggs upon the rocks, and got a quantity of dry\n\
\ sea-weed, and parched grass, which I designed to kindle the next day, and\n\
\ roast my eggs as well as I could, for I had about me my flint, steel,\n\
\ match, and burning-glass.  I lay all night in the cave where I had lodged\n\
\ my provisions.  My bed was the same dry grass and sea-weed which I\n\
\ intended for fuel.  I slept very little, for the disquiets of my mind\n\
\ prevailed over my weariness, and kept me awake.  I considered how\n\
\ impossible it was to preserve my life in so desolate a place, and how\n\
\ miserable my end must be: yet found myself so listless and desponding,\n\
\ that I had not the heart to rise; and before I could get spirits enough\n\
\ to creep out of my cave, the day was far advanced.  I walked awhile among\n\
\ the rocks: the sky was perfectly clear, and the sun so hot, that I was\n\
\ forced to turn my face from it: when all on a sudden it became obscure,\n\
\ as I thought, in a manner very different from what happens by the\n\
\ interposition of a cloud.  I turned back, and perceived a vast opaque\n\
\ body between me and the sun moving forwards towards the island: it seemed\n\
\ to be about two miles high, and hid the sun six or seven minutes; but I\n\
\ did not observe the air to be much colder, or the sky more darkened, than\n\
\ if I had stood under the shade of a mountain.  As it approached nearer\n\
\ over the place where I was, it appeared to be a firm substance, the\n\
\ bottom flat, smooth, and shining very bright, from the reflection of the\n\
\ sea below.  I stood upon a height about two hundred yards from the shore,\n\
\ and saw this vast body descending almost to a parallel with me, at less\n\
\ than an English mile distance.  I took out my pocket perspective, and\n\
\ could plainly discover numbers of people moving up and down the sides of\n\
\ it, which appeared to be sloping; but what those people where doing I was\n\
\ not able to distinguish.\n\
\ \n\
\ The natural love of life gave me some inward motion of joy, and I was\n\
\ ready to entertain a hope that this adventure might, some way or other,\n\
\ help to deliver me from the desolate place and condition I was in.  But\n\
\ at the same time the reader can hardly conceive my astonishment, to\n\
\ behold an island in the air, inhabited by men, who were able (as it\n\
\ should seem) to raise or sink, or put it into progressive motion, as they\n\
\ pleased.  But not being at that time in a disposition to philosophise\n\
\ upon this phenomenon, I rather chose to observe what course the island\n\
\ would take, because it seemed for awhile to stand still.  Yet soon after,\n\
\ it advanced nearer, and I could see the sides of it encompassed with\n\
\ several gradations of galleries, and stairs, at certain intervals, to\n\
\ descend from one to the other.  In the lowest gallery, I beheld some\n\
\ people fishing with long angling rods, and others looking on.  I waved my\n\
\ cap (for my hat was long since worn out) and my handkerchief toward the\n\
\ island; and upon its nearer approach, I called and shouted with the\n\
\ utmost strength of my voice; and then looking circumspectly, I beheld a\n\
\ crowd gather to that side which was most in my view.  I found by their\n\
\ pointing towards me and to each other, that they plainly discovered me,\n\
\ although they made no return to my shouting.  But I could see four or\n\
\ five men running in great haste, up the stairs, to the top of the island,\n\
\ who then disappeared.  I happened rightly to conjecture, that these were\n\
\ sent for orders to some person in authority upon this occasion.\n\
\ \n\
\ The number of people increased, and, in less than half all hour, the\n\
\ island was moved and raised in such a manner, that the lowest gallery\n\
\ appeared in a parallel of less then a hundred yards distance from the\n\
\ height where I stood.  I then put myself in the most supplicating\n\
\ posture, and spoke in the humblest accent, but received no answer.  Those\n\
\ who stood nearest over against me, seemed to be persons of distinction,\n\
\ as I supposed by their habit.  They conferred earnestly with each other,\n\
\ looking often upon me.  At length one of them called out in a clear,\n\
\ polite, smooth dialect, not unlike in sound to the Italian: and therefore\n\
\ I returned an answer in that language, hoping at least that the cadence\n\
\ might be more agreeable to his ears.  Although neither of us understood\n\
\ the other, yet my meaning was easily known, for the people saw the\n\
\ distress I was in.\n\
\ \n\
\ They made signs for me to come down from the rock, and go towards the\n\
\ shore, which I accordingly did; and the flying island being raised to a\n\
\ convenient height, the verge directly over me, a chain was let down from\n\
\ the lowest gallery, with a seat fastened to the bottom, to which I fixed\n\
\ myself, and was drawn up by pulleys.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER II.\n\
\ \n\
\ \n\
\ The humours and dispositions of the Laputians described.  An account of\n\
\ their learning.  Of the king and his court.  The author's reception\n\
\ there.  The inhabitants subject to fear and disquietudes.  An account of\n\
\ the women.\n\
\ \n\
\ At my alighting, I was surrounded with a crowd of people, but those who\n\
\ stood nearest seemed to be of better quality.  They beheld me with all\n\
\ the marks and circumstances of wonder; neither indeed was I much in their\n\
\ debt, having never till then seen a race of mortals so singular in their\n\
\ shapes, habits, and countenances.  Their heads were all reclined, either\n\
\ to the right, or the left; one of their eyes turned inward, and the other\n\
\ directly up to the zenith.  Their outward garments were adorned with the\n\
\ figures of suns, moons, and stars; interwoven with those of fiddles,\n\
\ flutes, harps, trumpets, guitars, harpsichords, and many other\n\
\ instruments of music, unknown to us in Europe.  I observed, here and\n\
\ there, many in the habit of servants, with a blown bladder, fastened like\n\
\ a flail to the end of a stick, which they carried in their hands.  In\n\
\ each bladder was a small quantity of dried peas, or little pebbles, as I\n\
\ was afterwards informed.  With these bladders, they now and then flapped\n\
\ the mouths and ears of those who stood near them, of which practice I\n\
\ could not then conceive the meaning.  It seems the minds of these people\n\
\ are so taken up with intense speculations, that they neither can speak,\n\
\ nor attend to the discourses of others, without being roused by some\n\
\ external taction upon the organs of speech and hearing; for which reason,\n\
\ those persons who are able to afford it always keep a flapper (the\n\
\ original is _climenole_) in their family, as one of their domestics; nor\n\
\ ever walk abroad, or make visits, without him.  And the business of this\n\
\ officer is, when two, three, or more persons are in company, gently to\n\
\ strike with his bladder the mouth of him who is to speak, and the right\n\
\ ear of him or them to whom the speaker addresses himself.  This flapper\n\
\ is likewise employed diligently to attend his master in his walks, and\n\
\ upon occasion to give him a soft flap on his eyes; because he is always\n\
\ so wrapped up in cogitation, that he is in manifest danger of falling\n\
\ down every precipice, and bouncing his head against every post; and in\n\
\ the streets, of justling others, or being justled himself into the\n\
\ kennel.\n\
\ \n\
\ It was necessary to give the reader this information, without which he\n\
\ would be at the same loss with me to understand the proceedings of these\n\
\ people, as they conducted me up the stairs to the top of the island, and\n\
\ from thence to the royal palace.  While we were ascending, they forgot\n\
\ several times what they were about, and left me to myself, till their\n\
\ memories were again roused by their flappers; for they appeared\n\
\ altogether unmoved by the sight of my foreign habit and countenance, and\n\
\ by the shouts of the vulgar, whose thoughts and minds were more\n\
\ disengaged.\n\
\ \n\
\ At last we entered the palace, and proceeded into the chamber of\n\
\ presence, where I saw the king seated on his throne, attended on each\n\
\ side by persons of prime quality.  Before the throne, was a large table\n\
\ filled with globes and spheres, and mathematical instruments of all\n\
\ kinds.  His majesty took not the least notice of us, although our\n\
\ entrance was not without sufficient noise, by the concourse of all\n\
\ persons belonging to the court.  But he was then deep in a problem; and\n\
\ we attended at least an hour, before he could solve it.  There stood by\n\
\ him, on each side, a young page with flaps in their hands, and when they\n\
\ saw he was at leisure, one of them gently struck his mouth, and the other\n\
\ his right ear; at which he startled like one awaked on the sudden, and\n\
\ looking towards me and the company I was in, recollected the occasion of\n\
\ our coming, whereof he had been informed before.  He spoke some words,\n\
\ whereupon immediately a young man with a flap came up to my side, and\n\
\ flapped me gently on the right ear; but I made signs, as well as I could,\n\
\ that I had no occasion for such an instrument; which, as I afterwards\n\
\ found, gave his majesty, and the whole court, a very mean opinion of my\n\
\ understanding.  The king, as far as I could conjecture, asked me several\n\
\ questions, and I addressed myself to him in all the languages I had.\n\
\ When it was found I could neither understand nor be understood, I was\n\
\ conducted by his order to an apartment in his palace (this prince being\n\
\ distinguished above all his predecessors for his hospitality to\n\
\ strangers), where two servants were appointed to attend me.  My dinner\n\
\ was brought, and four persons of quality, whom I remembered to have seen\n\
\ very near the king's person, did me the honour to dine with me.  We had\n\
\ two courses, of three dishes each.  In the first course, there was a\n\
\ shoulder of mutton cut into an equilateral triangle, a piece of beef into\n\
\ a rhomboides, and a pudding into a cycloid.  The second course was two\n\
\ ducks trussed up in the form of fiddles; sausages and puddings resembling\n\
\ flutes and hautboys, and a breast of veal in the shape of a harp.  The\n\
\ servants cut our bread into cones, cylinders, parallelograms, and several\n\
\ other mathematical figures.\n\
\ \n\
\ While we were at dinner, I made bold to ask the names of several things\n\
\ in their language, and those noble persons, by the assistance of their\n\
\ flappers, delighted to give me answers, hoping to raise my admiration of\n\
\ their great abilities if I could be brought to converse with them.  I was\n\
\ soon able to call for bread and drink, or whatever else I wanted.\n\
\ \n\
\ After dinner my company withdrew, and a person was sent to me by the\n\
\ king's order, attended by a flapper.  He brought with him pen, ink, and\n\
\ paper, and three or four books, giving me to understand by signs, that he\n\
\ was sent to teach me the language.  We sat together four hours, in which\n\
\ time I wrote down a great number of words in columns, with the\n\
\ translations over against them; I likewise made a shift to learn several\n\
\ short sentences; for my tutor would order one of my servants to fetch\n\
\ something, to turn about, to make a bow, to sit, or to stand, or walk,\n\
\ and the like.  Then I took down the sentence in writing.  He showed me\n\
\ also, in one of his books, the figures of the sun, moon, and stars, the\n\
\ zodiac, the tropics, and polar circles, together with the denominations\n\
\ of many plains and solids.  He gave me the names and descriptions of all\n\
\ the musical instruments, and the general terms of art in playing on each\n\
\ of them.  After he had left me, I placed all my words, with their\n\
\ interpretations, in alphabetical order.  And thus, in a few days, by the\n\
\ help of a very faithful memory, I got some insight into their language.\n\
\ The word, which I interpret the flying or floating island, is in the\n\
\ original _Laputa_, whereof I could never learn the true etymology.\n\
\ _Lap_, in the old obsolete language, signifies high; and _untuh_, a\n\
\ governor; from which they say, by corruption, was derived _Laputa_, from\n\
\ _Lapuntuh_.  But I do not approve of this derivation, which seems to be a\n\
\ little strained.  I ventured to offer to the learned among them a\n\
\ conjecture of my own, that Laputa was _quasi lap outed_; _lap_,\n\
\ signifying properly, the dancing of the sunbeams in the sea, and _outed_,\n\
\ a wing; which, however, I shall not obtrude, but submit to the judicious\n\
\ reader.\n\
\ \n\
\ Those to whom the king had entrusted me, observing how ill I was clad,\n\
\ ordered a tailor to come next morning, and take measure for a suit of\n\
\ clothes.  This operator did his office after a different manner from\n\
\ those of his trade in Europe.  He first took my altitude by a quadrant,\n\
\ and then, with a rule and compasses, described the dimensions and\n\
\ outlines of my whole body, all which he entered upon paper; and in six\n\
\ days brought my clothes very ill made, and quite out of shape, by\n\
\ happening to mistake a figure in the calculation.  But my comfort was,\n\
\ that I observed such accidents very frequent, and little regarded.\n\
\ \n\
\ During my confinement for want of clothes, and by an indisposition that\n\
\ held me some days longer, I much enlarged my dictionary; and when I went\n\
\ next to court, was able to understand many things the king spoke, and to\n\
\ return him some kind of answers.  His majesty had given orders, that the\n\
\ island should move north-east and by east, to the vertical point over\n\
\ Lagado, the metropolis of the whole kingdom below, upon the firm earth.\n\
\ It was about ninety leagues distant, and our voyage lasted four days and\n\
\ a half.  I was not in the least sensible of the progressive motion made\n\
\ in the air by the island.  On the second morning, about eleven o'clock,\n\
\ the king himself in person, attended by his nobility, courtiers, and\n\
\ officers, having prepared all their musical instruments, played on them\n\
\ for three hours without intermission, so that I was quite stunned with\n\
\ the noise; neither could I possibly guess the meaning, till my tutor\n\
\ informed me.  He said that, the people of their island had their ears\n\
\ adapted to hear \"the music of the spheres, which always played at certain\n\
\ periods, and the court was now prepared to bear their part, in whatever\n\
\ instrument they most excelled.\"\n\
\ \n\
\ In our journey towards Lagado, the capital city, his majesty ordered that\n\
\ the island should stop over certain towns and villages, from whence he\n\
\ might receive the petitions of his subjects.  And to this purpose,\n\
\ several packthreads were let down, with small weights at the bottom.  On\n\
\ these packthreads the people strung their petitions, which mounted up\n\
\ directly, like the scraps of paper fastened by school boys at the end of\n\
\ the string that holds their kite.  Sometimes we received wine and\n\
\ victuals from below, which were drawn up by pulleys.\n\
\ \n\
\ The knowledge I had in mathematics, gave me great assistance in acquiring\n\
\ their phraseology, which depended much upon that science, and music; and\n\
\ in the latter I was not unskilled.  Their ideas are perpetually\n\
\ conversant in lines and figures.  If they would, for example, praise the\n\
\ beauty of a woman, or any other animal, they describe it by rhombs,\n\
\ circles, parallelograms, ellipses, and other geometrical terms, or by\n\
\ words of art drawn from music, needless here to repeat.  I observed in\n\
\ the king's kitchen all sorts of mathematical and musical instruments,\n\
\ after the figures of which they cut up the joints that were served to his\n\
\ majesty's table.\n\
\ \n\
\ Their houses are very ill built, the walls bevil, without one right angle\n\
\ in any apartment; and this defect arises from the contempt they bear to\n\
\ practical geometry, which they despise as vulgar and mechanic; those\n\
\ instructions they give being too refined for the intellects of their\n\
\ workmen, which occasions perpetual mistakes.  And although they are\n\
\ dexterous enough upon a piece of paper, in the management of the rule,\n\
\ the pencil, and the divider, yet in the common actions and behaviour of\n\
\ life, I have not seen a more clumsy, awkward, and unhandy people, nor so\n\
\ slow and perplexed in their conceptions upon all other subjects, except\n\
\ those of mathematics and music.  They are very bad reasoners, and\n\
\ vehemently given to opposition, unless when they happen to be of the\n\
\ right opinion, which is seldom their case.  Imagination, fancy, and\n\
\ invention, they are wholly strangers to, nor have any words in their\n\
\ language, by which those ideas can be expressed; the whole compass of\n\
\ their thoughts and mind being shut up within the two forementioned\n\
\ sciences.\n\
\ \n\
\ Most of them, and especially those who deal in the astronomical part,\n\
\ have great faith in judicial astrology, although they are ashamed to own\n\
\ it publicly.  But what I chiefly admired, and thought altogether\n\
\ unaccountable, was the strong disposition I observed in them towards news\n\
\ and politics, perpetually inquiring into public affairs, giving their\n\
\ judgments in matters of state, and passionately disputing every inch of a\n\
\ party opinion.  I have indeed observed the same disposition among most of\n\
\ the mathematicians I have known in Europe, although I could never\n\
\ discover the least analogy between the two sciences; unless those people\n\
\ suppose, that because the smallest circle has as many degrees as the\n\
\ largest, therefore the regulation and management of the world require no\n\
\ more abilities than the handling and turning of a globe; but I rather\n\
\ take this quality to spring from a very common infirmity of human nature,\n\
\ inclining us to be most curious and conceited in matters where we have\n\
\ least concern, and for which we are least adapted by study or nature.\n\
\ \n\
\ These people are under continual disquietudes, never enjoying a minutes\n\
\ peace of mind; and their disturbances proceed from causes which very\n\
\ little affect the rest of mortals.  Their apprehensions arise from\n\
\ several changes they dread in the celestial bodies: for instance, that\n\
\ the earth, by the continual approaches of the sun towards it, must, in\n\
\ course of time, be absorbed, or swallowed up; that the face of the sun,\n\
\ will, by degrees, be encrusted with its own effluvia, and give no more\n\
\ light to the world; that the earth very narrowly escaped a brush from the\n\
\ tail of the last comet, which would have infallibly reduced it to ashes;\n\
\ and that the next, which they have calculated for one-and-thirty years\n\
\ hence, will probably destroy us.  For if, in its perihelion, it should\n\
\ approach within a certain degree of the sun (as by their calculations\n\
\ they have reason to dread) it will receive a degree of heat ten thousand\n\
\ times more intense than that of red hot glowing iron, and in its absence\n\
\ from the sun, carry a blazing tail ten hundred thousand and fourteen\n\
\ miles long, through which, if the earth should pass at the distance of\n\
\ one hundred thousand miles from the nucleus, or main body of the comet,\n\
\ it must in its passage be set on fire, and reduced to ashes: that the\n\
\ sun, daily spending its rays without any nutriment to supply them, will\n\
\ at last be wholly consumed and annihilated; which must be attended with\n\
\ the destruction of this earth, and of all the planets that receive their\n\
\ light from it.\n\
\ \n\
\ They are so perpetually alarmed with the apprehensions of these, and the\n\
\ like impending dangers, that they can neither sleep quietly in their\n\
\ beds, nor have any relish for the common pleasures and amusements of\n\
\ life.  When they meet an acquaintance in the morning, the first question\n\
\ is about the sun's health, how he looked at his setting and rising, and\n\
\ what hopes they have to avoid the stroke of the approaching comet.  This\n\
\ conversation they are apt to run into with the same temper that boys\n\
\ discover in delighting to hear terrible stories of spirits and\n\
\ hobgoblins, which they greedily listen to, and dare not go to bed for\n\
\ fear.\n\
\ \n\
\ The women of the island have abundance of vivacity: they, contemn their\n\
\ husbands, and are exceedingly fond of strangers, whereof there is always\n\
\ a considerable number from the continent below, attending at court,\n\
\ either upon affairs of the several towns and corporations, or their own\n\
\ particular occasions, but are much despised, because they want the same\n\
\ endowments.  Among these the ladies choose their gallants: but the\n\
\ vexation is, that they act with too much ease and security; for the\n\
\ husband is always so rapt in speculation, that the mistress and lover may\n\
\ proceed to the greatest familiarities before his face, if he be but\n\
\ provided with paper and implements, and without his flapper at his side.\n\
\ \n\
\ The wives and daughters lament their confinement to the island, although\n\
\ I think it the most delicious spot of ground in the world; and although\n\
\ they live here in the greatest plenty and magnificence, and are allowed\n\
\ to do whatever they please, they long to see the world, and take the\n\
\ diversions of the metropolis, which they are not allowed to do without a\n\
\ particular license from the king; and this is not easy to be obtained,\n\
\ because the people of quality have found, by frequent experience, how\n\
\ hard it is to persuade their women to return from below.  I was told that\n\
\ a great court lady, who had several children,--is married to the prime\n\
\ minister, the richest subject in the kingdom, a very graceful person,\n\
\ extremely fond of her, and lives in the finest palace of the\n\
\ island,--went down to Lagado on the pretence of health, there hid herself\n\
\ for several months, till the king sent a warrant to search for her; and\n\
\ she was found in an obscure eating-house all in rags, having pawned her\n\
\ clothes to maintain an old deformed footman, who beat her every day, and\n\
\ in whose company she was taken, much against her will.  And although her\n\
\ husband received her with all possible kindness, and without the least\n\
\ reproach, she soon after contrived to steal down again, with all her\n\
\ jewels, to the same gallant, and has not been heard of since.\n\
\ \n\
\ This may perhaps pass with the reader rather for an European or English\n\
\ story, than for one of a country so remote.  But he may please to\n\
\ consider, that the caprices of womankind are not limited by any climate\n\
\ or nation, and that they are much more uniform, than can be easily\n\
\ imagined.\n\
\ \n\
\ In about a month's time, I had made a tolerable proficiency in their\n\
\ language, and was able to answer most of the king's questions, when I had\n\
\ the honour to attend him.  His majesty discovered not the least curiosity\n\
\ to inquire into the laws, government, history, religion, or manners of\n\
\ the countries where I had been; but confined his questions to the state\n\
\ of mathematics, and received the account I gave him with great contempt\n\
\ and indifference, though often roused by his flapper on each side.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER III.\n\
\ \n\
\ \n\
\ A phenomenon solved by modern philosophy and astronomy.  The Laputians'\n\
\ great improvements in the latter.  The king's method of suppressing\n\
\ insurrections.\n\
\ \n\
\ I desired leave of this prince to see the curiosities of the island,\n\
\ which he was graciously pleased to grant, and ordered my tutor to attend\n\
\ me.  I chiefly wanted to know, to what cause, in art or in nature, it\n\
\ owed its several motions, whereof I will now give a philosophical account\n\
\ to the reader.\n\
\ \n\
\ The flying or floating island is exactly circular, its diameter 7837\n\
\ yards, or about four miles and a half, and consequently contains ten\n\
\ thousand acres.  It is three hundred yards thick.  The bottom, or under\n\
\ surface, which appears to those who view it below, is one even regular\n\
\ plate of adamant, shooting up to the height of about two hundred yards.\n\
\ Above it lie the several minerals in their usual order, and over all is a\n\
\ coat of rich mould, ten or twelve feet deep.  The declivity of the upper\n\
\ surface, from the circumference to the centre, is the natural cause why\n\
\ all the dews and rains, which fall upon the island, are conveyed in small\n\
\ rivulets toward the middle, where they are emptied into four large\n\
\ basins, each of about half a mile in circuit, and two hundred yards\n\
\ distant from the centre.  From these basins the water is continually\n\
\ exhaled by the sun in the daytime, which effectually prevents their\n\
\ overflowing.  Besides, as it is in the power of the monarch to raise the\n\
\ island above the region of clouds and vapours, he can prevent the falling\n\
\ of dews and rain whenever he pleases.  For the highest clouds cannot rise\n\
\ above two miles, as naturalists agree, at least they were never known to\n\
\ do so in that country.\n\
\ \n\
\ At the centre of the island there is a chasm about fifty yards in\n\
\ diameter, whence the astronomers descend into a large dome, which is\n\
\ therefore called _flandona gagnole_, or the astronomer's cave, situated\n\
\ at the depth of a hundred yards beneath the upper surface of the adamant.\n\
\ In this cave are twenty lamps continually burning, which, from the\n\
\ reflection of the adamant, cast a strong light into every part.  The\n\
\ place is stored with great variety of sextants, quadrants, telescopes,\n\
\ astrolabes, and other astronomical instruments.  But the greatest\n\
\ curiosity, upon which the fate of the island depends, is a loadstone of a\n\
\ prodigious size, in shape resembling a weaver's shuttle.  It is in length\n\
\ six yards, and in the thickest part at least three yards over.  This\n\
\ magnet is sustained by a very strong axle of adamant passing through its\n\
\ middle, upon which it plays, and is poised so exactly that the weakest\n\
\ hand can turn it.  It is hooped round with a hollow cylinder of adamant,\n\
\ four feet yards in diameter, placed horizontally, and supported by eight\n\
\ adamantine feet, each six yards high.  In the middle of the concave side,\n\
\ there is a groove twelve inches deep, in which the extremities of the\n\
\ axle are lodged, and turned round as there is occasion.\n\
\ \n\
\ The stone cannot be removed from its place by any force, because the hoop\n\
\ and its feet are one continued piece with that body of adamant which\n\
\ constitutes the bottom of the island.\n\
\ \n\
\ By means of this loadstone, the island is made to rise and fall, and move\n\
\ from one place to another.  For, with respect to that part of the earth\n\
\ over which the monarch presides, the stone is endued at one of its sides\n\
\ with an attractive power, and at the other with a repulsive.  Upon\n\
\ placing the magnet erect, with its attracting end towards the earth, the\n\
\ island descends; but when the repelling extremity points downwards, the\n\
\ island mounts directly upwards.  When the position of the stone is\n\
\ oblique, the motion of the island is so too: for in this magnet, the\n\
\ forces always act in lines parallel to its direction.\n\
\ \n\
\ By this oblique motion, the island is conveyed to different parts of the\n\
\ monarch's dominions.  To explain the manner of its progress, let _A_ _B_\n\
\ represent a line drawn across the dominions of Balnibarbi, let the line\n\
\ _c_ _d_ represent the loadstone, of which let _d_ be the repelling end,\n\
\ and _c_ the attracting end, the island being over _C_: let the stone be\n\
\ placed in position _c_ _d_, with its repelling end downwards; then the\n\
\ island will be driven upwards obliquely towards _D_.  When it is arrived\n\
\ at _D_, let the stone be turned upon its axle, till its attracting end\n\
\ points towards _E_, and then the island will be carried obliquely towards\n\
\ _E_; where, if the stone be again turned upon its axle till it stands in\n\
\ the position _E_ _F_, with its repelling point downwards, the island will\n\
\ rise obliquely towards _F_, where, by directing the attracting end\n\
\ towards _G_, the island may be carried to _G_, and from _G_ to _H_, by\n\
\ turning the stone, so as to make its repelling extremity to point\n\
\ directly downward.  And thus, by changing the situation of the stone, as\n\
\ often as there is occasion, the island is made to rise and fall by turns\n\
\ in an oblique direction, and by those alternate risings and fallings (the\n\
\ obliquity being not considerable) is conveyed from one part of the\n\
\ dominions to the other.\n\
\ \n\
\ But it must be observed, that this island cannot move beyond the extent\n\
\ of the dominions below, nor can it rise above the height of four miles.\n\
\ For which the astronomers (who have written large systems concerning the\n\
\ stone) assign the following reason: that the magnetic virtue does not\n\
\ extend beyond the distance of four miles, and that the mineral, which\n\
\ acts upon the stone in the bowels of the earth, and in the sea about six\n\
\ leagues distant from the shore, is not diffused through the whole globe,\n\
\ but terminated with the limits of the king's dominions; and it was easy,\n\
\ from the great advantage of such a superior situation, for a prince to\n\
\ bring under his obedience whatever country lay within the attraction of\n\
\ that magnet.\n\
\ \n\
\ When the stone is put parallel to the plane of the horizon, the island\n\
\ stands still; for in that case the extremities of it, being at equal\n\
\ distance from the earth, act with equal force, the one in drawing\n\
\ downwards, the other in pushing upwards, and consequently no motion can\n\
\ ensue.\n\
\ \n\
\ This loadstone is under the care of certain astronomers, who, from time\n\
\ to time, give it such positions as the monarch directs.  They spend the\n\
\ greatest part of their lives in observing the celestial bodies, which\n\
\ they do by the assistance of glasses, far excelling ours in goodness.\n\
\ For, although their largest telescopes do not exceed three feet, they\n\
\ magnify much more than those of a hundred with us, and show the stars\n\
\ with greater clearness.  This advantage has enabled them to extend their\n\
\ discoveries much further than our astronomers in Europe; for they have\n\
\ made a catalogue of ten thousand fixed stars, whereas the largest of ours\n\
\ do not contain above one third part of that number.  They have likewise\n\
\ discovered two lesser stars, or satellites, which revolve about Mars;\n\
\ whereof the innermost is distant from the centre of the primary planet\n\
\ exactly three of his diameters, and the outermost, five; the former\n\
\ revolves in the space of ten hours, and the latter in twenty-one and a\n\
\ half; so that the squares of their periodical times are very near in the\n\
\ same proportion with the cubes of their distance from the centre of Mars;\n\
\ which evidently shows them to be governed by the same law of gravitation\n\
\ that influences the other heavenly bodies.\n\
\ \n\
\ They have observed ninety-three different comets, and settled their\n\
\ periods with great exactness.  If this be true (and they affirm it with\n\
\ great confidence) it is much to be wished, that their observations were\n\
\ made public, whereby the theory of comets, which at present is very lame\n\
\ and defective, might be brought to the same perfection with other arts of\n\
\ astronomy.\n\
\ \n\
\ The king would be the most absolute prince in the universe, if he could\n\
\ but prevail on a ministry to join with him; but these having their\n\
\ estates below on the continent, and considering that the office of a\n\
\ favourite has a very uncertain tenure, would never consent to the\n\
\ enslaving of their country.\n\
\ \n\
\ If any town should engage in rebellion or mutiny, fall into violent\n\
\ factions, or refuse to pay the usual tribute, the king has two methods of\n\
\ reducing them to obedience.  The first and the mildest course is, by\n\
\ keeping the island hovering over such a town, and the lands about it,\n\
\ whereby he can deprive them of the benefit of the sun and the rain, and\n\
\ consequently afflict the inhabitants with dearth and diseases: and if the\n\
\ crime deserve it, they are at the same time pelted from above with great\n\
\ stones, against which they have no defence but by creeping into cellars\n\
\ or caves, while the roofs of their houses are beaten to pieces.  But if\n\
\ they still continue obstinate, or offer to raise insurrections, he\n\
\ proceeds to the last remedy, by letting the island drop directly upon\n\
\ their heads, which makes a universal destruction both of houses and men.\n\
\ However, this is an extremity to which the prince is seldom driven,\n\
\ neither indeed is he willing to put it in execution; nor dare his\n\
\ ministers advise him to an action, which, as it would render them odious\n\
\ to the people, so it would be a great damage to their own estates, which\n\
\ all lie below; for the island is the king's demesne.\n\
\ \n\
\ But there is still indeed a more weighty reason, why the kings of this\n\
\ country have been always averse from executing so terrible an action,\n\
\ unless upon the utmost necessity.  For, if the town intended to be\n\
\ destroyed should have in it any tall rocks, as it generally falls out in\n\
\ the larger cities, a situation probably chosen at first with a view to\n\
\ prevent such a catastrophe; or if it abound in high spires, or pillars of\n\
\ stone, a sudden fall might endanger the bottom or under surface of the\n\
\ island, which, although it consist, as I have said, of one entire\n\
\ adamant, two hundred yards thick, might happen to crack by too great a\n\
\ shock, or burst by approaching too near the fires from the houses below,\n\
\ as the backs, both of iron and stone, will often do in our chimneys.  Of\n\
\ all this the people are well apprised, and understand how far to carry\n\
\ their obstinacy, where their liberty or property is concerned.  And the\n\
\ king, when he is highest provoked, and most determined to press a city to\n\
\ rubbish, orders the island to descend with great gentleness, out of a\n\
\ pretence of tenderness to his people, but, indeed, for fear of breaking\n\
\ the adamantine bottom; in which case, it is the opinion of all their\n\
\ philosophers, that the loadstone could no longer hold it up, and the\n\
\ whole mass would fall to the ground.\n\
\ \n\
\ By a fundamental law of this realm, neither the king, nor either of his\n\
\ two eldest sons, are permitted to leave the island; nor the queen, till\n\
\ she is past child-bearing.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER IV.\n\
\ \n\
\ \n\
\ The author leaves Laputa; is conveyed to Balnibarbi; arrives at the\n\
\ metropolis.  A description of the metropolis, and the country adjoining.\n\
\ The author hospitably received by a great lord.  His conversation with\n\
\ that lord.\n\
\ \n\
\ Although I cannot say that I was ill treated in this island, yet I must\n\
\ confess I thought myself too much neglected, not without some degree of\n\
\ contempt; for neither prince nor people appeared to be curious in any\n\
\ part of knowledge, except mathematics and music, wherein I was far their\n\
\ inferior, and upon that account very little regarded.\n\
\ \n\
\ On the other side, after having seen all the curiosities of the island, I\n\
\ was very desirous to leave it, being heartily weary of those people.\n\
\ They were indeed excellent in two sciences for which I have great esteem,\n\
\ and wherein I am not unversed; but, at the same time, so abstracted and\n\
\ involved in speculation, that I never met with such disagreeable\n\
\ companions.  I conversed only with women, tradesmen, flappers, and\n\
\ court-pages, during two months of my abode there; by which, at last, I\n\
\ rendered myself extremely contemptible; yet these were the only people\n\
\ from whom I could ever receive a reasonable answer.\n\
\ \n\
\ I had obtained, by hard study, a good degree of knowledge in their\n\
\ language: I was weary of being confined to an island where I received so\n\
\ little countenance, and resolved to leave it with the first opportunity.\n\
\ \n\
\ There was a great lord at court, nearly related to the king, and for that\n\
\ reason alone used with respect.  He was universally reckoned the most\n\
\ ignorant and stupid person among them.  He had performed many eminent\n\
\ services for the crown, had great natural and acquired parts, adorned\n\
\ with integrity and honour; but so ill an ear for music, that his\n\
\ detractors reported, \"he had been often known to beat time in the wrong\n\
\ place;\" neither could his tutors, without extreme difficulty, teach him\n\
\ to demonstrate the most easy proposition in the mathematics.  He was\n\
\ pleased to show me many marks of favour, often did me the honour of a\n\
\ visit, desired to be informed in the affairs of Europe, the laws and\n\
\ customs, the manners and learning of the several countries where I had\n\
\ travelled.  He listened to me with great attention, and made very wise\n\
\ observations on all I spoke.  He had two flappers attending him for\n\
\ state, but never made use of them, except at court and in visits of\n\
\ ceremony, and would always command them to withdraw, when we were alone\n\
\ together.\n\
\ \n\
\ I entreated this illustrious person, to intercede in my behalf with his\n\
\ majesty, for leave to depart; which he accordingly did, as he was pleased\n\
\ to tell me, with regret: for indeed he had made me several offers very\n\
\ advantageous, which, however, I refused, with expressions of the highest\n\
\ acknowledgment.\n\
\ \n\
\ On the 16th of February I took leave of his majesty and the court.  The\n\
\ king made me a present to the value of about two hundred pounds English,\n\
\ and my protector, his kinsman, as much more, together with a letter of\n\
\ recommendation to a friend of his in Lagado, the metropolis.  The island\n\
\ being then hovering over a mountain about two miles from it, I was let\n\
\ down from the lowest gallery, in the same manner as I had been taken up.\n\
\ \n\
\ The continent, as far as it is subject to the monarch of the flying\n\
\ island, passes under the general name of _Balnibarbi_; and the\n\
\ metropolis, as I said before, is called _Lagado_.  I felt some little\n\
\ satisfaction in finding myself on firm ground.  I walked to the city\n\
\ without any concern, being clad like one of the natives, and sufficiently\n\
\ instructed to converse with them.  I soon found out the person's house to\n\
\ whom I was recommended, presented my letter from his friend the grandee\n\
\ in the island, and was received with much kindness.  This great lord,\n\
\ whose name was Munodi, ordered me an apartment in his own house, where I\n\
\ continued during my stay, and was entertained in a most hospitable\n\
\ manner.\n\
\ \n\
\ The next morning after my arrival, he took me in his chariot to see the\n\
\ town, which is about half the bigness of London; but the houses very\n\
\ strangely built, and most of them out of repair.  The people in the\n\
\ streets walked fast, looked wild, their eyes fixed, and were generally in\n\
\ rags.  We passed through one of the town gates, and went about three\n\
\ miles into the country, where I saw many labourers working with several\n\
\ sorts of tools in the ground, but was not able to conjecture what they\n\
\ were about: neither did observe any expectation either of corn or grass,\n\
\ although the soil appeared to be excellent.  I could not forbear admiring\n\
\ at these odd appearances, both in town and country; and I made bold to\n\
\ desire my conductor, that he would be pleased to explain to me, what\n\
\ could be meant by so many busy heads, hands, and faces, both in the\n\
\ streets and the fields, because I did not discover any good effects they\n\
\ produced; but, on the contrary, I never knew a soil so unhappily\n\
\ cultivated, houses so ill contrived and so ruinous, or a people whose\n\
\ countenances and habit expressed so much misery and want.\n\
\ \n\
\ This lord Munodi was a person of the first rank, and had been some years\n\
\ governor of Lagado; but, by a cabal of ministers, was discharged for\n\
\ insufficiency.  However, the king treated him with tenderness, as a\n\
\ well-meaning man, but of a low contemptible understanding.\n\
\ \n\
\ When I gave that free censure of the country and its inhabitants, he made\n\
\ no further answer than by telling me, \"that I had not been long enough\n\
\ among them to form a judgment; and that the different nations of the\n\
\ world had different customs;\" with other common topics to the same\n\
\ purpose.  But, when we returned to his palace, he asked me \"how I liked\n\
\ the building, what absurdities I observed, and what quarrel I had with\n\
\ the dress or looks of his domestics?\"  This he might safely do; because\n\
\ every thing about him was magnificent, regular, and polite.  I answered,\n\
\ \"that his excellency's prudence, quality, and fortune, had exempted him\n\
\ from those defects, which folly and beggary had produced in others.\"  He\n\
\ said, \"if I would go with him to his country-house, about twenty miles\n\
\ distant, where his estate lay, there would be more leisure for this kind\n\
\ of conversation.\"  I told his excellency \"that I was entirely at his\n\
\ disposal;\" and accordingly we set out next morning.\n\
\ \n\
\ During our journey he made me observe the several methods used by farmers\n\
\ in managing their lands, which to me were wholly unaccountable; for,\n\
\ except in some very few places, I could not discover one ear of corn or\n\
\ blade of grass.  But, in three hours travelling, the scene was wholly\n\
\ altered; we came into a most beautiful country; farmers' houses, at small\n\
\ distances, neatly built; the fields enclosed, containing vineyards,\n\
\ corn-grounds, and meadows.  Neither do I remember to have seen a more\n\
\ delightful prospect.  His excellency observed my countenance to clear up;\n\
\ he told me, with a sigh, \"that there his estate began, and would continue\n\
\ the same, till we should come to his house: that his countrymen ridiculed\n\
\ and despised him, for managing his affairs no better, and for setting so\n\
\ ill an example to the kingdom; which, however, was followed by very few,\n\
\ such as were old, and wilful, and weak like himself.\"\n\
\ \n\
\ We came at length to the house, which was indeed a noble structure, built\n\
\ according to the best rules of ancient architecture.  The fountains,\n\
\ gardens, walks, avenues, and groves, were all disposed with exact\n\
\ judgment and taste.  I gave due praises to every thing I saw, whereof his\n\
\ excellency took not the least notice till after supper; when, there being\n\
\ no third companion, he told me with a very melancholy air \"that he\n\
\ doubted he must throw down his houses in town and country, to rebuild\n\
\ them after the present mode; destroy all his plantations, and cast others\n\
\ into such a form as modern usage required, and give the same directions\n\
\ to all his tenants, unless he would submit to incur the censure of pride,\n\
\ singularity, affectation, ignorance, caprice, and perhaps increase his\n\
\ majesty's displeasure; that the admiration I appeared to be under would\n\
\ cease or diminish, when he had informed me of some particulars which,\n\
\ probably, I never heard of at court, the people there being too much\n\
\ taken up in their own speculations, to have regard to what passed here\n\
\ below.\"\n\
\ \n\
\ The sum of his discourse was to this effect: \"That about forty years ago,\n\
\ certain persons went up to Laputa, either upon business or diversion,\n\
\ and, after five months continuance, came back with a very little\n\
\ smattering in mathematics, but full of volatile spirits acquired in that\n\
\ airy region: that these persons, upon their return, began to dislike the\n\
\ management of every thing below, and fell into schemes of putting all\n\
\ arts, sciences, languages, and mechanics, upon a new foot.  To this end,\n\
\ they procured a royal patent for erecting an academy of projectors in\n\
\ Lagado; and the humour prevailed so strongly among the people, that there\n\
\ is not a town of any consequence in the kingdom without such an academy.\n\
\ In these colleges the professors contrive new rules and methods of\n\
\ agriculture and building, and new instruments, and tools for all trades\n\
\ and manufactures; whereby, as they undertake, one man shall do the work\n\
\ of ten; a palace may be built in a week, of materials so durable as to\n\
\ last for ever without repairing.  All the fruits of the earth shall come\n\
\ to maturity at whatever season we think fit to choose, and increase a\n\
\ hundred fold more than they do at present; with innumerable other happy\n\
\ proposals.  The only inconvenience is, that none of these projects are\n\
\ yet brought to perfection; and in the mean time, the whole country lies\n\
\ miserably waste, the houses in ruins, and the people without food or\n\
\ clothes.  By all which, instead of being discouraged, they are fifty\n\
\ times more violently bent upon prosecuting their schemes, driven equally\n\
\ on by hope and despair: that as for himself, being not of an enterprising\n\
\ spirit, he was content to go on in the old forms, to live in the houses\n\
\ his ancestors had built, and act as they did, in every part of life,\n\
\ without innovation: that some few other persons of quality and gentry had\n\
\ done the same, but were looked on with an eye of contempt and ill-will,\n\
\ as enemies to art, ignorant, and ill common-wealth's men, preferring\n\
\ their own ease and sloth before the general improvement of their\n\
\ country.\"\n\
\ \n\
\ His lordship added, \"That he would not, by any further particulars,\n\
\ prevent the pleasure I should certainly take in viewing the grand\n\
\ academy, whither he was resolved I should go.\"  He only desired me to\n\
\ observe a ruined building, upon the side of a mountain about three miles\n\
\ distant, of which he gave me this account: \"That he had a very convenient\n\
\ mill within half a mile of his house, turned by a current from a large\n\
\ river, and sufficient for his own family, as well as a great number of\n\
\ his tenants; that about seven years ago, a club of those projectors came\n\
\ to him with proposals to destroy this mill, and build another on the side\n\
\ of that mountain, on the long ridge whereof a long canal must be cut, for\n\
\ a repository of water, to be conveyed up by pipes and engines to supply\n\
\ the mill, because the wind and air upon a height agitated the water, and\n\
\ thereby made it fitter for motion, and because the water, descending down\n\
\ a declivity, would turn the mill with half the current of a river whose\n\
\ course is more upon a level.\"  He said, \"that being then not very well\n\
\ with the court, and pressed by many of his friends, he complied with the\n\
\ proposal; and after employing a hundred men for two years, the work\n\
\ miscarried, the projectors went off, laying the blame entirely upon him,\n\
\ railing at him ever since, and putting others upon the same experiment,\n\
\ with equal assurance of success, as well as equal disappointment.\"\n\
\ \n\
\ In a few days we came back to town; and his excellency, considering the\n\
\ bad character he had in the academy, would not go with me himself, but\n\
\ recommended me to a friend of his, to bear me company thither.  My lord\n\
\ was pleased to represent me as a great admirer of projects, and a person\n\
\ of much curiosity and easy belief; which, indeed, was not without truth;\n\
\ for I had myself been a sort of projector in my younger days.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER V.\n\
\ \n\
\ \n\
\ The author permitted to see the grand academy of Lagado.  The academy\n\
\ largely described.  The arts wherein the professors employ themselves.\n\
\ \n\
\ This academy is not an entire single building, but a continuation of\n\
\ several houses on both sides of a street, which growing waste, was\n\
\ purchased and applied to that use.\n\
\ \n\
\ I was received very kindly by the warden, and went for many days to the\n\
\ academy.  Every room has in it one or more projectors; and I believe I\n\
\ could not be in fewer than five hundred rooms.\n\
\ \n\
\ The first man I saw was of a meagre aspect, with sooty hands and face,\n\
\ his hair and beard long, ragged, and singed in several places.  His\n\
\ clothes, shirt, and skin, were all of the same colour.  He has been eight\n\
\ years upon a project for extracting sunbeams out of cucumbers, which were\n\
\ to be put in phials hermetically sealed, and let out to warm the air in\n\
\ raw inclement summers.  He told me, he did not doubt, that, in eight\n\
\ years more, he should be able to supply the governor's gardens with\n\
\ sunshine, at a reasonable rate: but he complained that his stock was low,\n\
\ and entreated me \"to give him something as an encouragement to ingenuity,\n\
\ especially since this had been a very dear season for cucumbers.\"  I made\n\
\ him a small present, for my lord had furnished me with money on purpose,\n\
\ because he knew their practice of begging from all who go to see them.\n\
\ \n\
\ I went into another chamber, but was ready to hasten back, being almost\n\
\ overcome with a horrible stink.  My conductor pressed me forward,\n\
\ conjuring me in a whisper \"to give no offence, which would be highly\n\
\ resented;\" and therefore I durst not so much as stop my nose.  The\n\
\ projector of this cell was the most ancient student of the academy; his\n\
\ face and beard were of a pale yellow; his hands and clothes daubed over\n\
\ with filth.  When I was presented to him, he gave me a close embrace, a\n\
\ compliment I could well have excused.  His employment, from his first\n\
\ coming into the academy, was an operation to reduce human excrement to\n\
\ its original food, by separating the several parts, removing the tincture\n\
\ which it receives from the gall, making the odour exhale, and scumming\n\
\ off the saliva.  He had a weekly allowance, from the society, of a vessel\n\
\ filled with human ordure, about the bigness of a Bristol barrel.\n\
\ \n\
\ I saw another at work to calcine ice into gunpowder; who likewise showed\n\
\ me a treatise he had written concerning the malleability of fire, which\n\
\ he intended to publish.\n\
\ \n\
\ There was a most ingenious architect, who had contrived a new method for\n\
\ building houses, by beginning at the roof, and working downward to the\n\
\ foundation; which he justified to me, by the like practice of those two\n\
\ prudent insects, the bee and the spider.\n\
\ \n\
\ There was a man born blind, who had several apprentices in his own\n\
\ condition: their employment was to mix colours for painters, which their\n\
\ master taught them to distinguish by feeling and smelling.  It was indeed\n\
\ my misfortune to find them at that time not very perfect in their\n\
\ lessons, and the professor himself happened to be generally mistaken.\n\
\ This artist is much encouraged and esteemed by the whole fraternity.\n\
\ \n\
\ In another apartment I was highly pleased with a projector who had found\n\
\ a device of ploughing the ground with hogs, to save the charges of\n\
\ ploughs, cattle, and labour.  The method is this: in an acre of ground\n\
\ you bury, at six inches distance and eight deep, a quantity of acorns,\n\
\ dates, chestnuts, and other mast or vegetables, whereof these animals are\n\
\ fondest; then you drive six hundred or more of them into the field,\n\
\ where, in a few days, they will root up the whole ground in search of\n\
\ their food, and make it fit for sowing, at the same time manuring it with\n\
\ their dung: it is true, upon experiment, they found the charge and\n\
\ trouble very great, and they had little or no crop.  However it is not\n\
\ doubted, that this invention may be capable of great improvement.\n\
\ \n\
\ I went into another room, where the walls and ceiling were all hung round\n\
\ with cobwebs, except a narrow passage for the artist to go in and out.\n\
\ At my entrance, he called aloud to me, \"not to disturb his webs.\"  He\n\
\ lamented \"the fatal mistake the world had been so long in, of using\n\
\ silkworms, while we had such plenty of domestic insects who infinitely\n\
\ excelled the former, because they understood how to weave, as well as\n\
\ spin.\"  And he proposed further, \"that by employing spiders, the charge\n\
\ of dyeing silks should be wholly saved;\" whereof I was fully convinced,\n\
\ when he showed me a vast number of flies most beautifully coloured,\n\
\ wherewith he fed his spiders, assuring us \"that the webs would take a\n\
\ tincture from them; and as he had them of all hues, he hoped to fit\n\
\ everybody's fancy, as soon as he could find proper food for the flies, of\n\
\ certain gums, oils, and other glutinous matter, to give a strength and\n\
\ consistence to the threads.\"\n\
\ \n\
\ There was an astronomer, who had undertaken to place a sun-dial upon the\n\
\ great weathercock on the town-house, by adjusting the annual and diurnal\n\
\ motions of the earth and sun, so as to answer and coincide with all\n\
\ accidental turnings of the wind.\n\
\ \n\
\ I was complaining of a small fit of the colic, upon which my conductor\n\
\ led me into a room where a great physician resided, who was famous for\n\
\ curing that disease, by contrary operations from the same instrument.  He\n\
\ had a large pair of bellows, with a long slender muzzle of ivory: this he\n\
\ conveyed eight inches up the anus, and drawing in the wind, he affirmed\n\
\ he could make the guts as lank as a dried bladder.  But when the disease\n\
\ was more stubborn and violent, he let in the muzzle while the bellows\n\
\ were full of wind, which he discharged into the body of the patient; then\n\
\ withdrew the instrument to replenish it, clapping his thumb strongly\n\
\ against the orifice of then fundament; and this being repeated three or\n\
\ four times, the adventitious wind would rush out, bringing the noxious\n\
\ along with it, (like water put into a pump), and the patient recovered.\n\
\ I saw him try both experiments upon a dog, but could not discern any\n\
\ effect from the former.  After the latter the animal was ready to burst,\n\
\ and made so violent a discharge as was very offensive to me and my\n\
\ companion.  The dog died on the spot, and we left the doctor endeavouring\n\
\ to recover him, by the same operation.\n\
\ \n\
\ I visited many other apartments, but shall not trouble my reader with all\n\
\ the curiosities I observed, being studious of brevity.\n\
\ \n\
\ I had hitherto seen only one side of the academy, the other being\n\
\ appropriated to the advancers of speculative learning, of whom I shall\n\
\ say something, when I have mentioned one illustrious person more, who is\n\
\ called among them \"the universal artist.\"  He told us \"he had been thirty\n\
\ years employing his thoughts for the improvement of human life.\"  He had\n\
\ two large rooms full of wonderful curiosities, and fifty men at work.\n\
\ Some were condensing air into a dry tangible substance, by extracting the\n\
\ nitre, and letting the aqueous or fluid particles percolate; others\n\
\ softening marble, for pillows and pin-cushions; others petrifying the\n\
\ hoofs of a living horse, to preserve them from foundering.  The artist\n\
\ himself was at that time busy upon two great designs; the first, to sow\n\
\ land with chaff, wherein he affirmed the true seminal virtue to be\n\
\ contained, as he demonstrated by several experiments, which I was not\n\
\ skilful enough to comprehend.  The other was, by a certain composition of\n\
\ gums, minerals, and vegetables, outwardly applied, to prevent the growth\n\
\ of wool upon two young lambs; and he hoped, in a reasonable time to\n\
\ propagate the breed of naked sheep, all over the kingdom.\n\
\ \n\
\ We crossed a walk to the other part of the academy, where, as I have\n\
\ already said, the projectors in speculative learning resided.\n\
\ \n\
\ The first professor I saw, was in a very large room, with forty pupils\n\
\ about him.  After salutation, observing me to look earnestly upon a\n\
\ frame, which took up the greatest part of both the length and breadth of\n\
\ the room, he said, \"Perhaps I might wonder to see him employed in a\n\
\ project for improving speculative knowledge, by practical and mechanical\n\
\ operations.  But the world would soon be sensible of its usefulness; and\n\
\ he flattered himself, that a more noble, exalted thought never sprang in\n\
\ any other man's head.  Every one knew how laborious the usual method is\n\
\ of attaining to arts and sciences; whereas, by his contrivance, the most\n\
\ ignorant person, at a reasonable charge, and with a little bodily labour,\n\
\ might write books in philosophy, poetry, politics, laws, mathematics, and\n\
\ theology, without the least assistance from genius or study.\"  He then\n\
\ led me to the frame, about the sides, whereof all his pupils stood in\n\
\ ranks.  It was twenty feet square, placed in the middle of the room.  The\n\
\ superfices was composed of several bits of wood, about the bigness of a\n\
\ die, but some larger than others.  They were all linked together by\n\
\ slender wires.  These bits of wood were covered, on every square, with\n\
\ paper pasted on them; and on these papers were written all the words of\n\
\ their language, in their several moods, tenses, and declensions; but\n\
\ without any order.  The professor then desired me \"to observe; for he was\n\
\ going to set his engine at work.\"  The pupils, at his command, took each\n\
\ of them hold of an iron handle, whereof there were forty fixed round the\n\
\ edges of the frame; and giving them a sudden turn, the whole disposition\n\
\ of the words was entirely changed.  He then commanded six-and-thirty of\n\
\ the lads, to read the several lines softly, as they appeared upon the\n\
\ frame; and where they found three or four words together that might make\n\
\ part of a sentence, they dictated to the four remaining boys, who were\n\
\ scribes.  This work was repeated three or four times, and at every turn,\n\
\ the engine was so contrived, that the words shifted into new places, as\n\
\ the square bits of wood moved upside down.\n\
\ \n\
\                            [Picture: The frame]\n\
\ \n\
\ Six hours a day the young students were employed in this labour; and the\n\
\ professor showed me several volumes in large folio, already collected, of\n\
\ broken sentences, which he intended to piece together, and out of those\n\
\ rich materials, to give the world a complete body of all arts and\n\
\ sciences; which, however, might be still improved, and much expedited, if\n\
\ the public would raise a fund for making and employing five hundred such\n\
\ frames in Lagado, and oblige the managers to contribute in common their\n\
\ several collections.\n\
\ \n\
\ He assured me \"that this invention had employed all his thoughts from his\n\
\ youth; that he had emptied the whole vocabulary into his frame, and made\n\
\ the strictest computation of the general proportion there is in books\n\
\ between the numbers of particles, nouns, and verbs, and other parts of\n\
\ speech.\"\n\
\ \n\
\ I made my humblest acknowledgment to this illustrious person, for his\n\
\ great communicativeness; and promised, \"if ever I had the good fortune to\n\
\ return to my native country, that I would do him justice, as the sole\n\
\ inventor of this wonderful machine;\" the form and contrivance of which I\n\
\ desired leave to delineate on paper, as in the figure here annexed.  I\n\
\ told him, \"although it were the custom of our learned in Europe to steal\n\
\ inventions from each other, who had thereby at least this advantage, that\n\
\ it became a controversy which was the right owner; yet I would take such\n\
\ caution, that he should have the honour entire, without a rival.\"\n\
\ \n\
\ We next went to the school of languages, where three professors sat in\n\
\ consultation upon improving that of their own country.\n\
\ \n\
\ The first project was, to shorten discourse, by cutting polysyllables\n\
\ into one, and leaving out verbs and participles, because, in reality, all\n\
\ things imaginable are but norms.\n\
\ \n\
\ The other project was, a scheme for entirely abolishing all words\n\
\ whatsoever; and this was urged as a great advantage in point of health,\n\
\ as well as brevity.  For it is plain, that every word we speak is, in\n\
\ some degree, a diminution of our lunge by corrosion, and, consequently,\n\
\ contributes to the shortening of our lives.  An expedient was therefore\n\
\ offered, \"that since words are only names for things, it would be more\n\
\ convenient for all men to carry about them such things as were necessary\n\
\ to express a particular business they are to discourse on.\"  And this\n\
\ invention would certainly have taken place, to the great ease as well as\n\
\ health of the subject, if the women, in conjunction with the vulgar and\n\
\ illiterate, had not threatened to raise a rebellion unless they might be\n\
\ allowed the liberty to speak with their tongues, after the manner of\n\
\ their forefathers; such constant irreconcilable enemies to science are\n\
\ the common people.  However, many of the most learned and wise adhere to\n\
\ the new scheme of expressing themselves by things; which has only this\n\
\ inconvenience attending it, that if a man's business be very great, and\n\
\ of various kinds, he must be obliged, in proportion, to carry a greater\n\
\ bundle of things upon his back, unless he can afford one or two strong\n\
\ servants to attend him.  I have often beheld two of those sages almost\n\
\ sinking under the weight of their packs, like pedlars among us, who, when\n\
\ they met in the street, would lay down their loads, open their sacks, and\n\
\ hold conversation for an hour together; then put up their implements,\n\
\ help each other to resume their burdens, and take their leave.\n\
\ \n\
\ But for short conversations, a man may carry implements in his pockets,\n\
\ and under his arms, enough to supply him; and in his house, he cannot be\n\
\ at a loss.  Therefore the room where company meet who practise this art,\n\
\ is full of all things, ready at hand, requisite to furnish matter for\n\
\ this kind of artificial converse.\n\
\ \n\
\ Another great advantage proposed by this invention was, that it would\n\
\ serve as a universal language, to be understood in all civilised nations,\n\
\ whose goods and utensils are generally of the same kind, or nearly\n\
\ resembling, so that their uses might easily be comprehended.  And thus\n\
\ ambassadors would be qualified to treat with foreign princes, or\n\
\ ministers of state, to whose tongues they were utter strangers.\n\
\ \n\
\ I was at the mathematical school, where the master taught his pupils\n\
\ after a method scarce imaginable to us in Europe.  The proposition, and\n\
\ demonstration, were fairly written on a thin wafer, with ink composed of\n\
\ a cephalic tincture.  This, the student was to swallow upon a fasting\n\
\ stomach, and for three days following, eat nothing but bread and water.\n\
\ As the wafer digested, the tincture mounted to his brain, bearing the\n\
\ proposition along with it.  But the success has not hitherto been\n\
\ answerable, partly by some error in the _quantum_ or composition, and\n\
\ partly by the perverseness of lads, to whom this bolus is so nauseous,\n\
\ that they generally steal aside, and discharge it upwards, before it can\n\
\ operate; neither have they been yet persuaded to use so long an\n\
\ abstinence, as the prescription requires.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER VI.\n\
\ \n\
\ \n\
\ A further account of the academy.  The author proposes some improvements,\n\
\ which are honourably received.\n\
\ \n\
\ In the school of political projectors, I was but ill entertained; the\n\
\ professors appearing, in my judgment, wholly out of their senses, which\n\
\ is a scene that never fails to make me melancholy.  These unhappy people\n\
\ were proposing schemes for persuading monarchs to choose favourites upon\n\
\ the score of their wisdom, capacity, and virtue; of teaching ministers to\n\
\ consult the public good; of rewarding merit, great abilities, eminent\n\
\ services; of instructing princes to know their true interest, by placing\n\
\ it on the same foundation with that of their people; of choosing for\n\
\ employments persons qualified to exercise them, with many other wild,\n\
\ impossible chimeras, that never entered before into the heart of man to\n\
\ conceive; and confirmed in me the old observation, \"that there is nothing\n\
\ so extravagant and irrational, which some philosophers have not\n\
\ maintained for truth.\"\n\
\ \n\
\ But, however, I shall so far do justice to this part of the Academy, as\n\
\ to acknowledge that all of them were not so visionary.  There was a most\n\
\ ingenious doctor, who seemed to be perfectly versed in the whole nature\n\
\ and system of government.  This illustrious person had very usefully\n\
\ employed his studies, in finding out effectual remedies for all diseases\n\
\ and corruptions to which the several kinds of public administration are\n\
\ subject, by the vices or infirmities of those who govern, as well as by\n\
\ the licentiousness of those who are to obey.  For instance: whereas all\n\
\ writers and reasoners have agreed, that there is a strict universal\n\
\ resemblance between the natural and the political body; can there be any\n\
\ thing more evident, than that the health of both must be preserved, and\n\
\ the diseases cured, by the same prescriptions?  It is allowed, that\n\
\ senates and great councils are often troubled with redundant, ebullient,\n\
\ and other peccant humours; with many diseases of the head, and more of\n\
\ the heart; with strong convulsions, with grievous contractions of the\n\
\ nerves and sinews in both hands, but especially the right; with spleen,\n\
\ flatus, vertigos, and deliriums; with scrofulous tumours, full of fetid\n\
\ purulent matter; with sour frothy ructations: with canine appetites, and\n\
\ crudeness of digestion, besides many others, needless to mention.  This\n\
\ doctor therefore proposed, \"that upon the meeting of the senate, certain\n\
\ physicians should attend it the three first days of their sitting, and at\n\
\ the close of each day's debate feel the pulses of every senator; after\n\
\ which, having maturely considered and consulted upon the nature of the\n\
\ several maladies, and the methods of cure, they should on the fourth day\n\
\ return to the senate house, attended by their apothecaries stored with\n\
\ proper medicines; and before the members sat, administer to each of them\n\
\ lenitives, aperitives, abstersives, corrosives, restringents,\n\
\ palliatives, laxatives, cephalalgics, icterics, apophlegmatics,\n\
\ acoustics, as their several cases required; and, according as these\n\
\ medicines should operate, repeat, alter, or omit them, at the next\n\
\ meeting.\"\n\
\ \n\
\ This project could not be of any great expense to the public; and might\n\
\ in my poor opinion, be of much use for the despatch of business, in those\n\
\ countries where senates have any share in the legislative power; beget\n\
\ unanimity, shorten debates, open a few mouths which are now closed, and\n\
\ close many more which are now open; curb the petulancy of the young, and\n\
\ correct the positiveness of the old; rouse the stupid, and damp the pert.\n\
\ \n\
\ Again: because it is a general complaint, that the favourites of princes\n\
\ are troubled with short and weak memories; the same doctor proposed,\n\
\ \"that whoever attended a first minister, after having told his business,\n\
\ with the utmost brevity and in the plainest words, should, at his\n\
\ departure, give the said minister a tweak by the nose, or a kick in the\n\
\ belly, or tread on his corns, or lug him thrice by both ears, or run a\n\
\ pin into his breech; or pinch his arm black and blue, to prevent\n\
\ forgetfulness; and at every levee day, repeat the same operation, till\n\
\ the business were done, or absolutely refused.\"\n\
\ \n\
\ He likewise directed, \"that every senator in the great council of a\n\
\ nation, after he had delivered his opinion, and argued in the defence of\n\
\ it, should be obliged to give his vote directly contrary; because if that\n\
\ were done, the result would infallibly terminate in the good of the\n\
\ public.\"\n\
\ \n\
\ When parties in a state are violent, he offered a wonderful contrivance\n\
\ to reconcile them.  The method is this: You take a hundred leaders of\n\
\ each party; you dispose them into couples of such whose heads are nearest\n\
\ of a size; then let two nice operators saw off the occiput of each couple\n\
\ at the same time, in such a manner that the brain may be equally divided.\n\
\ Let the occiputs, thus cut off, be interchanged, applying each to the\n\
\ head of his opposite party-man.  It seems indeed to be a work that\n\
\ requires some exactness, but the professor assured us, \"that if it were\n\
\ dexterously performed, the cure would be infallible.\"  For he argued\n\
\ thus: \"that the two half brains being left to debate the matter between\n\
\ themselves within the space of one skull, would soon come to a good\n\
\ understanding, and produce that moderation, as well as regularity of\n\
\ thinking, so much to be wished for in the heads of those, who imagine\n\
\ they come into the world only to watch and govern its motion: and as to\n\
\ the difference of brains, in quantity or quality, among those who are\n\
\ directors in faction, the doctor assured us, from his own knowledge, that\n\
\ \"it was a perfect trifle.\"\n\
\ \n\
\ I heard a very warm debate between two professors, about the most\n\
\ commodious and effectual ways and means of raising money, without\n\
\ grieving the subject.  The first affirmed, \"the justest method would be,\n\
\ to lay a certain tax upon vices and folly; and the sum fixed upon every\n\
\ man to be rated, after the fairest manner, by a jury of his neighbours.\"\n\
\ The second was of an opinion directly contrary; \"to tax those qualities\n\
\ of body and mind, for which men chiefly value themselves; the rate to be\n\
\ more or less, according to the degrees of excelling; the decision whereof\n\
\ should be left entirely to their own breast.\"  The highest tax was upon\n\
\ men who are the greatest favourites of the other sex, and the\n\
\ assessments, according to the number and nature of the favours they have\n\
\ received; for which, they are allowed to be their own vouchers.  Wit,\n\
\ valour, and politeness, were likewise proposed to be largely taxed, and\n\
\ collected in the same manner, by every person's giving his own word for\n\
\ the quantum of what he possessed.  But as to honour, justice, wisdom, and\n\
\ learning, they should not be taxed at all; because they are\n\
\ qualifications of so singular a kind, that no man will either allow them\n\
\ in his neighbour or value them in himself.\n\
\ \n\
\ The women were proposed to be taxed according to their beauty and skill\n\
\ in dressing, wherein they had the same privilege with the men, to be\n\
\ determined by their own judgment.  But constancy, chastity, good sense,\n\
\ and good nature, were not rated, because they would not bear the charge\n\
\ of collecting.\n\
\ \n\
\ To keep senators in the interest of the crown, it was proposed that the\n\
\ members should raffle for employment; every man first taking an oath, and\n\
\ giving security, that he would vote for the court, whether he won or not;\n\
\ after which, the losers had, in their turn, the liberty of raffling upon\n\
\ the next vacancy.  Thus, hope and expectation would be kept alive; none\n\
\ would complain of broken promises, but impute their disappointments\n\
\ wholly to fortune, whose shoulders are broader and stronger than those of\n\
\ a ministry.\n\
\ \n\
\ Another professor showed me a large paper of instructions for discovering\n\
\ plots and conspiracies against the government.  He advised great\n\
\ statesmen to examine into the diet of all suspected persons; their times\n\
\ of eating; upon which side they lay in bed; with which hand they wipe\n\
\ their posteriors; take a strict view of their excrements, and, from the\n\
\ colour, the odour, the taste, the consistence, the crudeness or maturity\n\
\ of digestion, form a judgment of their thoughts and designs; because men\n\
\ are never so serious, thoughtful, and intent, as when they are at stool,\n\
\ which he found by frequent experiment; for, in such conjunctures, when he\n\
\ used, merely as a trial, to consider which was the best way of murdering\n\
\ the king, his ordure would have a tincture of green; but quite different,\n\
\ when he thought only of raising an insurrection, or burning the\n\
\ metropolis.\n\
\ \n\
\ The whole discourse was written with great acuteness, containing many\n\
\ observations, both curious and useful for politicians; but, as I\n\
\ conceived, not altogether complete.  This I ventured to tell the author,\n\
\ and offered, if he pleased, to supply him with some additions.  He\n\
\ received my proposition with more compliance than is usual among writers,\n\
\ especially those of the projecting species, professing \"he would be glad\n\
\ to receive further information.\"\n\
\ \n\
\ I told him, \"that in the kingdom of Tribnia, {454a} by the natives called\n\
\ Langdon, {454b} where I had sojourned some time in my travels, the bulk\n\
\ of the people consist in a manner wholly of discoverers, witnesses,\n\
\ informers, accusers, prosecutors, evidences, swearers, together with\n\
\ their several subservient and subaltern instruments, all under the\n\
\ colours, the conduct, and the pay of ministers of state, and their\n\
\ deputies.  The plots, in that kingdom, are usually the workmanship of\n\
\ those persons who desire to raise their own characters of profound\n\
\ politicians; to restore new vigour to a crazy administration; to stifle\n\
\ or divert general discontents; to fill their coffers with forfeitures;\n\
\ and raise, or sink the opinion of public credit, as either shall best\n\
\ answer their private advantage.  It is first agreed and settled among\n\
\ them, what suspected persons shall be accused of a plot; then, effectual\n\
\ care is taken to secure all their letters and papers, and put the owners\n\
\ in chains.  These papers are delivered to a set of artists, very\n\
\ dexterous in finding out the mysterious meanings of words, syllables, and\n\
\ letters: for instance, they can discover a close stool, to signify a\n\
\ privy council; a flock of geese, a senate; a lame dog, an invader; the\n\
\ plague, a standing army; a buzzard, a prime minister; the gout, a high\n\
\ priest; a gibbet, a secretary of state; a chamber pot, a committee of\n\
\ grandees; a sieve, a court lady; a broom, a revolution; a mouse-trap, an\n\
\ employment; a bottomless pit, a treasury; a sink, a court; a cap and\n\
\ bells, a favourite; a broken reed, a court of justice; an empty tun, a\n\
\ general; a running sore, the administration. {455}\n\
\ \n\
\ \"When this method fails, they have two others more effectual, which the\n\
\ learned among them call acrostics and anagrams.  First, they can decipher\n\
\ all initial letters into political meanings.  Thus _N_, shall signify a\n\
\ plot; _B_, a regiment of horse; _L_, a fleet at sea; or, secondly, by\n\
\ transposing the letters of the alphabet in any suspected paper, they can\n\
\ lay open the deepest designs of a discontented party.  So, for example,\n\
\ if I should say, in a letter to a friend, 'Our brother Tom has just got\n\
\ the piles,' a skilful decipherer would discover, that the same letters\n\
\ which compose that sentence, may be analysed into the following words,\n\
\ 'Resist ---, a plot is brought home--The tour.'  And this is the\n\
\ anagrammatic method.\"\n\
\ \n\
\ The professor made me great acknowledgments for communicating these\n\
\ observations, and promised to make honourable mention of me in his\n\
\ treatise.\n\
\ \n\
\ I saw nothing in this country that could invite me to a longer\n\
\ continuance, and began to think of returning home to England.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER VII.\n\
\ \n\
\ \n\
\ The author leaves Lagado: arrives at Maldonada.  No ship ready.  He takes\n\
\ a short voyage to Glubbdubdrib.  His reception by the governor.\n\
\ \n\
\ The continent, of which this kingdom is apart, extends itself, as I have\n\
\ reason to believe, eastward, to that unknown tract of America westward of\n\
\ California; and north, to the Pacific Ocean, which is not above a hundred\n\
\ and fifty miles from Lagado; where there is a good port, and much\n\
\ commerce with the great island of Luggnagg, situated to the north-west\n\
\ about 29 degrees north latitude, and 140 longitude.  This island of\n\
\ Luggnagg stands south-eastward of Japan, about a hundred leagues distant.\n\
\ There is a strict alliance between the Japanese emperor and the king of\n\
\ Luggnagg; which affords frequent opportunities of sailing from one island\n\
\ to the other.  I determined therefore to direct my course this way, in\n\
\ order to my return to Europe.  I hired two mules, with a guide, to show\n\
\ me the way, and carry my small baggage.  I took leave of my noble\n\
\ protector, who had shown me so much favour, and made me a generous\n\
\ present at my departure.\n\
\ \n\
\ My journey was without any accident or adventure worth relating.  When I\n\
\ arrived at the port of Maldonada (for so it is called) there was no ship\n\
\ in the harbour bound for Luggnagg, nor likely to be in some time.  The\n\
\ town is about as large as Portsmouth.  I soon fell into some\n\
\ acquaintance, and was very hospitably received.  A gentleman of\n\
\ distinction said to me, \"that since the ships bound for Luggnagg could\n\
\ not be ready in less than a month, it might be no disagreeable amusement\n\
\ for me to take a trip to the little island of Glubbdubdrib, about five\n\
\ leagues off to the south-west.\"  He offered himself and a friend to\n\
\ accompany me, and that I should be provided with a small convenient bark\n\
\ for the voyage.\n\
\ \n\
\ Glubbdubdrib, as nearly as I can interpret the word, signifies the island\n\
\ of sorcerers or magicians.  It is about one third as large as the Isle of\n\
\ Wight, and extremely fruitful: it is governed by the head of a certain\n\
\ tribe, who are all magicians.  This tribe marries only among each other,\n\
\ and the eldest in succession is prince or governor.  He has a noble\n\
\ palace, and a park of about three thousand acres, surrounded by a wall of\n\
\ hewn stone twenty feet high.  In this park are several small enclosures\n\
\ for cattle, corn, and gardening.\n\
\ \n\
\ The governor and his family are served and attended by domestics of a\n\
\ kind somewhat unusual.  By his skill in necromancy he has a power of\n\
\ calling whom he pleases from the dead, and commanding their service for\n\
\ twenty-four hours, but no longer; nor can he call the same persons up\n\
\ again in less than three months, except upon very extraordinary\n\
\ occasions.\n\
\ \n\
\ When we arrived at the island, which was about eleven in the morning, one\n\
\ of the gentlemen who accompanied me went to the governor, and desired\n\
\ admittance for a stranger, who came on purpose to have the honour of\n\
\ attending on his highness.  This was immediately granted, and we all\n\
\ three entered the gate of the palace between two rows of guards, armed\n\
\ and dressed after a very antic manner, and with something in their\n\
\ countenances that made my flesh creep with a horror I cannot express.  We\n\
\ passed through several apartments, between servants of the same sort,\n\
\ ranked on each side as before, till we came to the chamber of presence;\n\
\ where, after three profound obeisances, and a few general questions, we\n\
\ were permitted to sit on three stools, near the lowest step of his\n\
\ highness's throne.  He understood the language of Balnibarbi, although it\n\
\ was different from that of this island.  He desired me to give him some\n\
\ account of my travels; and, to let me see that I should be treated\n\
\ without ceremony, he dismissed all his attendants with a turn of his\n\
\ finger; at which, to my great astonishment, they vanished in an instant,\n\
\ like visions in a dream when we awake on a sudden.  I could not recover\n\
\ myself in some time, till the governor assured me, \"that I should receive\n\
\ no hurt:\" and observing my two companions to be under no concern, who had\n\
\ been often entertained in the same manner, I began to take courage, and\n\
\ related to his highness a short history of my several adventures; yet not\n\
\ without some hesitation, and frequently looking behind me to the place\n\
\ where I had seen those domestic spectres.  I had the honour to dine with\n\
\ the governor, where a new set of ghosts served up the meat, and waited at\n\
\ table.  I now observed myself to be less terrified than I had been in the\n\
\ morning.  I stayed till sunset, but humbly desired his highness to excuse\n\
\ me for not accepting his invitation of lodging in the palace.  My two\n\
\ friends and I lay at a private house in the town adjoining, which is the\n\
\ capital of this little island; and the next morning we returned to pay\n\
\ our duty to the governor, as he was pleased to command us.\n\
\ \n\
\ After this manner we continued in the island for ten days, most part of\n\
\ every day with the governor, and at night in our lodging.  I soon grew so\n\
\ familiarized to the sight of spirits, that after the third or fourth time\n\
\ they gave me no emotion at all: or, if I had any apprehensions left, my\n\
\ curiosity prevailed over them.  For his highness the governor ordered me\n\
\ \"to call up whatever persons I would choose to name, and in whatever\n\
\ numbers, among all the dead from the beginning of the world to the\n\
\ present time, and command them to answer any questions I should think fit\n\
\ to ask; with this condition, that my questions must be confined within\n\
\ the compass of the times they lived in.  And one thing I might depend\n\
\ upon, that they would certainly tell me the truth, for lying was a talent\n\
\ of no use in the lower world.\"\n\
\ \n\
\ I made my humble acknowledgments to his highness for so great a favour.\n\
\ We were in a chamber, from whence there was a fair prospect into the\n\
\ park.  And because my first inclination was to be entertained with scenes\n\
\ of pomp and magnificence, I desired to see Alexander the Great at the\n\
\ head of his army, just after the battle of Arbela: which, upon a motion\n\
\ of the governor's finger, immediately appeared in a large field, under\n\
\ the window where we stood.  Alexander was called up into the room: it was\n\
\ with great difficulty that I understood his Greek, and had but little of\n\
\ my own.  He assured me upon his honour \"that he was not poisoned, but\n\
\ died of a bad fever by excessive drinking.\"\n\
\ \n\
\ Next, I saw Hannibal passing the Alps, who told me \"he had not a drop of\n\
\ vinegar in his camp.\"\n\
\ \n\
\ I saw Caesar and Pompey at the head of their troops, just ready to\n\
\ engage.  I saw the former, in his last great triumph.  I desired that the\n\
\ senate of Rome might appear before me, in one large chamber, and an\n\
\ assembly of somewhat a later age in counterview, in another.  The first\n\
\ seemed to be an assembly of heroes and demigods; the other, a knot of\n\
\ pedlars, pick-pockets, highwayman, and bullies.\n\
\ \n\
\ The governor, at my request, gave the sign for Caesar and Brutus to\n\
\ advance towards us.  I was struck with a profound veneration at the sight\n\
\ of Brutus, and could easily discover the most consummate virtue, the\n\
\ greatest intrepidity and firmness of mind, the truest love of his\n\
\ country, and general benevolence for mankind, in every lineament of his\n\
\ countenance.  I observed, with much pleasure, that these two persons were\n\
\ in good intelligence with each other; and Caesar freely confessed to me,\n\
\ \"that the greatest actions of his own life were not equal, by many\n\
\ degrees, to the glory of taking it away.\"  I had the honour to have much\n\
\ conversation with Brutus; and was told, \"that his ancestor Junius,\n\
\ Socrates, Epaminondas, Cato the younger, Sir Thomas More, and himself\n\
\ were perpetually together:\" a sextumvirate, to which all the ages of the\n\
\ world cannot add a seventh.\n\
\ \n\
\ It would be tedious to trouble the reader with relating what vast numbers\n\
\ of illustrious persons were called up to gratify that insatiable desire I\n\
\ had to see the world in every period of antiquity placed before me.  I\n\
\ chiefly fed mine eyes with beholding the destroyers of tyrants and\n\
\ usurpers, and the restorers of liberty to oppressed and injured nations.\n\
\ But it is impossible to express the satisfaction I received in my own\n\
\ mind, after such a manner as to make it a suitable entertainment to the\n\
\ reader.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER VIII.\n\
\ \n\
\ \n\
\ A further account of Glubbdubdrib.  Ancient and modern history corrected.\n\
\ \n\
\ Having a desire to see those ancients who were most renowned for wit and\n\
\ learning, I set apart one day on purpose.  I proposed that Homer and\n\
\ Aristotle might appear at the head of all their commentators; but these\n\
\ were so numerous, that some hundreds were forced to attend in the court,\n\
\ and outward rooms of the palace.  I knew, and could distinguish those two\n\
\ heroes, at first sight, not only from the crowd, but from each other.\n\
\ Homer was the taller and comelier person of the two, walked very erect\n\
\ for one of his age, and his eyes were the most quick and piercing I ever\n\
\ beheld.  Aristotle stooped much, and made use of a staff.  His visage was\n\
\ meagre, his hair lank and thin, and his voice hollow.  I soon discovered\n\
\ that both of them were perfect strangers to the rest of the company, and\n\
\ had never seen or heard of them before; and I had a whisper from a ghost\n\
\ who shall be nameless, \"that these commentators always kept in the most\n\
\ distant quarters from their principals, in the lower world, through a\n\
\ consciousness of shame and guilt, because they had so horribly\n\
\ misrepresented the meaning of those authors to posterity.\"  I introduced\n\
\ Didymus and Eustathius to Homer, and prevailed on him to treat them\n\
\ better than perhaps they deserved, for he soon found they wanted a genius\n\
\ to enter into the spirit of a poet.  But Aristotle was out of all\n\
\ patience with the account I gave him of Scotus and Ramus, as I presented\n\
\ them to him; and he asked them, \"whether the rest of the tribe were as\n\
\ great dunces as themselves?\"\n\
\ \n\
\ I then desired the governor to call up Descartes and Gassendi, with whom\n\
\ I prevailed to explain their systems to Aristotle.  This great\n\
\ philosopher freely acknowledged his own mistakes in natural philosophy,\n\
\ because he proceeded in many things upon conjecture, as all men must do;\n\
\ and he found that Gassendi, who had made the doctrine of Epicurus as\n\
\ palatable as he could, and the vortices of Descartes, were equally to be\n\
\ exploded.  He predicted the same fate to _attraction_, whereof the\n\
\ present learned are such zealous asserters.  He said, \"that new systems\n\
\ of nature were but new fashions, which would vary in every age; and even\n\
\ those, who pretend to demonstrate them from mathematical principles,\n\
\ would flourish but a short period of time, and be out of vogue when that\n\
\ was determined.\"\n\
\ \n\
\ I spent five days in conversing with many others of the ancient learned.\n\
\ I saw most of the first Roman emperors.  I prevailed on the governor to\n\
\ call up Heliogabalus's cooks to dress us a dinner, but they could not\n\
\ show us much of their skill, for want of materials.  A helot of Agesilaus\n\
\ made us a dish of Spartan broth, but I was not able to get down a second\n\
\ spoonful.\n\
\ \n\
\ The two gentlemen, who conducted me to the island, were pressed by their\n\
\ private affairs to return in three days, which I employed in seeing some\n\
\ of the modern dead, who had made the greatest figure, for two or three\n\
\ hundred years past, in our own and other countries of Europe; and having\n\
\ been always a great admirer of old illustrious families, I desired the\n\
\ governor would call up a dozen or two of kings, with their ancestors in\n\
\ order for eight or nine generations.  But my disappointment was grievous\n\
\ and unexpected.  For, instead of a long train with royal diadems, I saw\n\
\ in one family two fiddlers, three spruce courtiers, and an Italian\n\
\ prelate.  In another, a barber, an abbot, and two cardinals.  I have too\n\
\ great a veneration for crowned heads, to dwell any longer on so nice a\n\
\ subject.  But as to counts, marquises, dukes, earls, and the like, I was\n\
\ not so scrupulous.  And I confess, it was not without some pleasure, that\n\
\ I found myself able to trace the particular features, by which certain\n\
\ families are distinguished, up to their originals.  I could plainly\n\
\ discover whence one family derives a long chin; why a second has abounded\n\
\ with knaves for two generations, and fools for two more; why a third\n\
\ happened to be crack-brained, and a fourth to be sharpers; whence it\n\
\ came, what Polydore Virgil says of a certain great house, _Nec vir\n\
\ fortis_, _nec foemina casta_; how cruelty, falsehood, and cowardice, grew\n\
\ to be characteristics by which certain families are distinguished as much\n\
\ as by their coats of arms; who first brought the pox into a noble house,\n\
\ which has lineally descended scrofulous tumours to their posterity.\n\
\ Neither could I wonder at all this, when I saw such an interruption of\n\
\ lineages, by pages, lackeys, valets, coachmen, gamesters, fiddlers,\n\
\ players, captains, and pickpockets.\n\
\ \n\
\ I was chiefly disgusted with modern history.  For having strictly\n\
\ examined all the persons of greatest name in the courts of princes, for a\n\
\ hundred years past, I found how the world had been misled by prostitute\n\
\ writers, to ascribe the greatest exploits in war, to cowards; the wisest\n\
\ counsel, to fools; sincerity, to flatterers; Roman virtue, to betrayers\n\
\ of their country; piety, to atheists; chastity, to sodomites; truth, to\n\
\ informers: how many innocent and excellent persons had been condemned to\n\
\ death or banishment by the practising of great ministers upon the\n\
\ corruption of judges, and the malice of factions: how many villains had\n\
\ been exalted to the highest places of trust, power, dignity, and profit:\n\
\ how great a share in the motions and events of courts, councils, and\n\
\ senates might be challenged by bawds, whores, pimps, parasites, and\n\
\ buffoons.  How low an opinion I had of human wisdom and integrity, when I\n\
\ was truly informed of the springs and motives of great enterprises and\n\
\ revolutions in the world, and of the contemptible accidents to which they\n\
\ owed their success.\n\
\ \n\
\ Here I discovered the roguery and ignorance of those who pretend to write\n\
\ anecdotes, or secret history; who send so many kings to their graves with\n\
\ a cup of poison; will repeat the discourse between a prince and chief\n\
\ minister, where no witness was by; unlock the thoughts and cabinets of\n\
\ ambassadors and secretaries of state; and have the perpetual misfortune\n\
\ to be mistaken.  Here I discovered the true causes of many great events\n\
\ that have surprised the world; how a whore can govern the back-stairs,\n\
\ the back-stairs a council, and the council a senate.  A general\n\
\ confessed, in my presence, \"that he got a victory purely by the force of\n\
\ cowardice and ill conduct;\" and an admiral, \"that, for want of proper\n\
\ intelligence, he beat the enemy, to whom he intended to betray the\n\
\ fleet.\"  Three kings protested to me, \"that in their whole reigns they\n\
\ never did once prefer any person of merit, unless by mistake, or\n\
\ treachery of some minister in whom they confided; neither would they do\n\
\ it if they were to live again:\" and they showed, with great strength of\n\
\ reason, \"that the royal throne could not be supported without corruption,\n\
\ because that positive, confident, restiff temper, which virtue infused\n\
\ into a man, was a perpetual clog to public business.\"\n\
\ \n\
\ I had the curiosity to inquire in a particular manner, by what methods\n\
\ great numbers had procured to themselves high titles of honour, and\n\
\ prodigious estates; and I confined my inquiry to a very modern period:\n\
\ however, without grating upon present times, because I would be sure to\n\
\ give no offence even to foreigners (for I hope the reader need not be\n\
\ told, that I do not in the least intend my own country, in what I say\n\
\ upon this occasion,) a great number of persons concerned were called up;\n\
\ and, upon a very slight examination, discovered such a scene of infamy,\n\
\ that I cannot reflect upon it without some seriousness.  Perjury,\n\
\ oppression, subornation, fraud, pandarism, and the like infirmities, were\n\
\ among the most excusable arts they had to mention; and for these I gave,\n\
\ as it was reasonable, great allowance.  But when some confessed they owed\n\
\ their greatness and wealth to sodomy, or incest; others, to the\n\
\ prostituting of their own wives and daughters; others, to the betraying\n\
\ of their country or their prince; some, to poisoning; more to the\n\
\ perverting of justice, in order to destroy the innocent, I hope I may be\n\
\ pardoned, if these discoveries inclined me a little to abate of that\n\
\ profound veneration, which I am naturally apt to pay to persons of high\n\
\ rank, who ought to be treated with the utmost respect due to their\n\
\ sublime dignity, by us their inferiors.\n\
\ \n\
\ I had often read of some great services done to princes and states, and\n\
\ desired to see the persons by whom those services were performed.  Upon\n\
\ inquiry I was told, \"that their names were to be found on no record,\n\
\ except a few of them, whom history has represented as the vilest of\n\
\ rogues and traitors.\"  As to the rest, I had never once heard of them.\n\
\ They all appeared with dejected looks, and in the meanest habit; most of\n\
\ them telling me, \"they died in poverty and disgrace, and the rest on a\n\
\ scaffold or a gibbet.\"\n\
\ \n\
\ Among others, there was one person, whose case appeared a little\n\
\ singular.  He had a youth about eighteen years old standing by his side.\n\
\ He told me, \"he had for many years been commander of a ship; and in the\n\
\ sea fight at Actium had the good fortune to break through the enemy's\n\
\ great line of battle, sink three of their capital ships, and take a\n\
\ fourth, which was the sole cause of Antony's flight, and of the victory\n\
\ that ensued; that the youth standing by him, his only son, was killed in\n\
\ the action.\"  He added, \"that upon the confidence of some merit, the war\n\
\ being at an end, he went to Rome, and solicited at the court of Augustus\n\
\ to be preferred to a greater ship, whose commander had been killed; but,\n\
\ without any regard to his pretensions, it was given to a boy who had\n\
\ never seen the sea, the son of Libertina, who waited on one of the\n\
\ emperor's mistresses.  Returning back to his own vessel, he was charged\n\
\ with neglect of duty, and the ship given to a favourite page of\n\
\ Publicola, the vice-admiral; whereupon he retired to a poor farm at a\n\
\ great distance from Rome, and there ended his life.\"  I was so curious to\n\
\ know the truth of this story, that I desired Agrippa might be called, who\n\
\ was admiral in that fight.  He appeared, and confirmed the whole account:\n\
\ but with much more advantage to the captain, whose modesty had extenuated\n\
\ or concealed a great part of his merit.\n\
\ \n\
\ I was surprised to find corruption grown so high and so quick in that\n\
\ empire, by the force of luxury so lately introduced; which made me less\n\
\ wonder at many parallel cases in other countries, where vices of all\n\
\ kinds have reigned so much longer, and where the whole praise, as well as\n\
\ pillage, has been engrossed by the chief commander, who perhaps had the\n\
\ least title to either.\n\
\ \n\
\ As every person called up made exactly the same appearance he had done in\n\
\ the world, it gave me melancholy reflections to observe how much the race\n\
\ of human kind was degenerated among us within these hundred years past;\n\
\ how the pox, under all its consequences and denominations had altered\n\
\ every lineament of an English countenance; shortened the size of bodies,\n\
\ unbraced the nerves, relaxed the sinews and muscles, introduced a sallow\n\
\ complexion, and rendered the flesh loose and rancid.\n\
\ \n\
\ I descended so low, as to desire some English yeoman of the old stamp\n\
\ might be summoned to appear; once so famous for the simplicity of their\n\
\ manners, diet, and dress; for justice in their dealings; for their true\n\
\ spirit of liberty; for their valour, and love of their country.  Neither\n\
\ could I be wholly unmoved, after comparing the living with the dead, when\n\
\ I considered how all these pure native virtues were prostituted for a\n\
\ piece of money by their grand-children; who, in selling their votes and\n\
\ managing at elections, have acquired every vice and corruption that can\n\
\ possibly be learned in a court.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER IX.\n\
\ \n\
\ \n\
\ The author returns to Maldonada.  Sails to the kingdom of Luggnagg.  The\n\
\ author confined.  He is sent for to court.  The manner of his admittance.\n\
\ The king's great lenity to his subjects.\n\
\ \n\
\ The day of our departure being come, I took leave of his highness, the\n\
\ Governor of Glubbdubdrib, and returned with my two companions to\n\
\ Maldonada, where, after a fortnight's waiting, a ship was ready to sail\n\
\ for Luggnagg.  The two gentlemen, and some others, were so generous and\n\
\ kind as to furnish me with provisions, and see me on board.  I was a\n\
\ month in this voyage.  We had one violent storm, and were under a\n\
\ necessity of steering westward to get into the trade wind, which holds\n\
\ for above sixty leagues.  On the 21st of April, 1708, we sailed into the\n\
\ river of Clumegnig, which is a seaport town, at the south-east point of\n\
\ Luggnagg.  We cast anchor within a league of the town, and made a signal\n\
\ for a pilot.  Two of them came on board in less than half an hour, by\n\
\ whom we were guided between certain shoals and rocks, which are very\n\
\ dangerous in the passage, to a large basin, where a fleet may ride in\n\
\ safety within a cable's length of the town-wall.\n\
\ \n\
\ Some of our sailors, whether out of treachery or inadvertence, had\n\
\ informed the pilots \"that I was a stranger, and great traveller;\" whereof\n\
\ these gave notice to a custom-house officer, by whom I was examined very\n\
\ strictly upon my landing.  This officer spoke to me in the language of\n\
\ Balnibarbi, which, by the force of much commerce, is generally understood\n\
\ in that town, especially by seamen and those employed in the customs.  I\n\
\ gave him a short account of some particulars, and made my story as\n\
\ plausible and consistent as I could; but I thought it necessary to\n\
\ disguise my country, and call myself a Hollander; because my intentions\n\
\ were for Japan, and I knew the Dutch were the only Europeans permitted to\n\
\ enter into that kingdom.  I therefore told the officer, \"that having been\n\
\ shipwrecked on the coast of Balnibarbi, and cast on a rock, I was\n\
\ received up into Laputa, or the flying island (of which he had often\n\
\ heard), and was now endeavouring to get to Japan, whence I might find a\n\
\ convenience of returning to my own country.\"  The officer said, \"I must\n\
\ be confined till he could receive orders from court, for which he would\n\
\ write immediately, and hoped to receive an answer in a fortnight.\"  I was\n\
\ carried to a convenient lodging with a sentry placed at the door;\n\
\ however, I had the liberty of a large garden, and was treated with\n\
\ humanity enough, being maintained all the time at the king's charge.  I\n\
\ was invited by several persons, chiefly out of curiosity, because it was\n\
\ reported that I came from countries very remote, of which they had never\n\
\ heard.\n\
\ \n\
\ I hired a young man, who came in the same ship, to be an interpreter; he\n\
\ was a native of Luggnagg, but had lived some years at Maldonada, and was\n\
\ a perfect master of both languages.  By his assistance, I was able to\n\
\ hold a conversation with those who came to visit me; but this consisted\n\
\ only of their questions, and my answers.\n\
\ \n\
\ The despatch came from court about the time we expected.  It contained a\n\
\ warrant for conducting me and my retinue to _Traldragdubh_, or\n\
\ _Trildrogdrib_ (for it is pronounced both ways as near as I can\n\
\ remember), by a party of ten horse.  All my retinue was that poor lad for\n\
\ an interpreter, whom I persuaded into my service, and, at my humble\n\
\ request, we had each of us a mule to ride on.  A messenger was despatched\n\
\ half a day's journey before us, to give the king notice of my approach,\n\
\ and to desire, \"that his majesty would please to appoint a day and hour,\n\
\ when it would by his gracious pleasure that I might have the honour to\n\
\ lick the dust before his footstool.\"  This is the court style, and I\n\
\ found it to be more than matter of form: for, upon my admittance two days\n\
\ after my arrival, I was commanded to crawl upon my belly, and lick the\n\
\ floor as I advanced; but, on account of my being a stranger, care was\n\
\ taken to have it made so clean, that the dust was not offensive.\n\
\ However, this was a peculiar grace, not allowed to any but persons of the\n\
\ highest rank, when they desire an admittance.  Nay, sometimes the floor\n\
\ is strewed with dust on purpose, when the person to be admitted happens\n\
\ to have powerful enemies at court; and I have seen a great lord with his\n\
\ mouth so crammed, that when he had crept to the proper distance from the\n\
\ throne; he was not able to speak a word.  Neither is there any remedy;\n\
\ because it is capital for those, who receive an audience to spit or wipe\n\
\ their mouths in his majesty's presence.  There is indeed another custom,\n\
\ which I cannot altogether approve of: when the king has a mind to put any\n\
\ of his nobles to death in a gentle indulgent manner, he commands the\n\
\ floor to be strewed with a certain brown powder of a deadly composition,\n\
\ which being licked up, infallibly kills him in twenty-four hours.  But in\n\
\ justice to this prince's great clemency, and the care he has of his\n\
\ subjects' lives (wherein it were much to be wished that the Monarchs of\n\
\ Europe would imitate him), it must be mentioned for his honour, that\n\
\ strict orders are given to have the infected parts of the floor well\n\
\ washed after every such execution, which, if his domestics neglect, they\n\
\ are in danger of incurring his royal displeasure.  I myself heard him\n\
\ give directions, that one of his pages should be whipped, whose turn it\n\
\ was to give notice about washing the floor after an execution, but\n\
\ maliciously had omitted it; by which neglect a young lord of great hopes,\n\
\ coming to an audience, was unfortunately poisoned, although the king at\n\
\ that time had no design against his life.  But this good prince was so\n\
\ gracious as to forgive the poor page his whipping, upon promise that he\n\
\ would do so no more, without special orders.\n\
\ \n\
\ To return from this digression.  When I had crept within four yards of\n\
\ the throne, I raised myself gently upon my knees, and then striking my\n\
\ forehead seven times against the ground, I pronounced the following\n\
\ words, as they had been taught me the night before, _Inckpling\n\
\ gloffthrobb squut serummblhiop mlashnalt zwin tnodbalkuffh slhiophad\n\
\ gurdlubh asht_.  This is the compliment, established by the laws of the\n\
\ land, for all persons admitted to the king's presence.  It may be\n\
\ rendered into English thus: \"May your celestial majesty outlive the sun,\n\
\ eleven moons and a half!\"  To this the king returned some answer, which,\n\
\ although I could not understand, yet I replied as I had been directed:\n\
\ _Fluft drin yalerick dwuldom prastrad mirpush_, which properly signifies,\n\
\ \"My tongue is in the mouth of my friend;\" and by this expression was\n\
\ meant, that I desired leave to bring my interpreter; whereupon the young\n\
\ man already mentioned was accordingly introduced, by whose intervention I\n\
\ answered as many questions as his majesty could put in above an hour.  I\n\
\ spoke in the Balnibarbian tongue, and my interpreter delivered my meaning\n\
\ in that of Luggnagg.\n\
\ \n\
\ The king was much delighted with my company, and ordered his\n\
\ _bliffmarklub_, or high-chamberlain, to appoint a lodging in the court\n\
\ for me and my interpreter; with a daily allowance for my table, and a\n\
\ large purse of gold for my common expenses.\n\
\ \n\
\ I staid three months in this country, out of perfect obedience to his\n\
\ majesty; who was pleased highly to favour me, and made me very honourable\n\
\ offers.  But I thought it more consistent with prudence and justice to\n\
\ pass the remainder of my days with my wife and family.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER X.\n\
\ \n\
\ \n\
\ The Luggnaggians commended.  A particular description of the Struldbrugs,\n\
\ with many conversations between the author and some eminent persons upon\n\
\ that subject.\n\
\ \n\
\ The Luggnaggians are a polite and generous people; and although they are\n\
\ not without some share of that pride which is peculiar to all Eastern\n\
\ countries, yet they show themselves courteous to strangers, especially\n\
\ such who are countenanced by the court.  I had many acquaintance, and\n\
\ among persons of the best fashion; and being always attended by my\n\
\ interpreter, the conversation we had was not disagreeable.\n\
\ \n\
\ One day, in much good company, I was asked by a person of quality,\n\
\ \"whether I had seen any of their _struldbrugs_, or immortals?\"  I said,\n\
\ \"I had not;\" and desired he would explain to me \"what he meant by such an\n\
\ appellation, applied to a mortal creature.\"  He told me \"that sometimes,\n\
\ though very rarely, a child happened to be born in a family, with a red\n\
\ circular spot in the forehead, directly over the left eyebrow, which was\n\
\ an infallible mark that it should never die.\"  The spot, as he described\n\
\ it, \"was about the compass of a silver threepence, but in the course of\n\
\ time grew larger, and changed its colour; for at twelve years old it\n\
\ became green, so continued till five and twenty, then turned to a deep\n\
\ blue: at five and forty it grew coal black, and as large as an English\n\
\ shilling; but never admitted any further alteration.\"  He said, \"these\n\
\ births were so rare, that he did not believe there could be above eleven\n\
\ hundred struldbrugs, of both sexes, in the whole kingdom; of which he\n\
\ computed about fifty in the metropolis, and, among the rest, a young girl\n\
\ born; about three years ago: that these productions were not peculiar to\n\
\ any family, but a mere effect of chance; and the children of the\n\
\ _struldbrugs_ themselves were equally mortal with the rest of the\n\
\ people.\"\n\
\ \n\
\ I freely own myself to have been struck with inexpressible delight, upon\n\
\ hearing this account: and the person who gave it me happening to\n\
\ understand the Balnibarbian language, which I spoke very well, I could\n\
\ not forbear breaking out into expressions, perhaps a little too\n\
\ extravagant.  I cried out, as in a rapture, \"Happy nation, where every\n\
\ child hath at least a chance for being immortal!  Happy people, who enjoy\n\
\ so many living examples of ancient virtue, and have masters ready to\n\
\ instruct them in the wisdom of all former ages! but happiest, beyond all\n\
\ comparison, are those excellent _struldbrugs_, who, being born exempt\n\
\ from that universal calamity of human nature, have their minds free and\n\
\ disengaged, without the weight and depression of spirits caused by the\n\
\ continual apprehensions of death!\"  I discovered my admiration that I had\n\
\ not observed any of these illustrious persons at court; the black spot on\n\
\ the forehead being so remarkable a distinction, that I could not have\n\
\ easily overlooked it: and it was impossible that his majesty, a most\n\
\ judicious prince, should not provide himself with a good number of such\n\
\ wise and able counsellors.  Yet perhaps the virtue of those reverend\n\
\ sages was too strict for the corrupt and libertine manners of a court:\n\
\ and we often find by experience, that young men are too opinionated and\n\
\ volatile to be guided by the sober dictates of their seniors.  However,\n\
\ since the king was pleased to allow me access to his royal person, I was\n\
\ resolved, upon the very first occasion, to deliver my opinion to him on\n\
\ this matter freely and at large, by the help of my interpreter; and\n\
\ whether he would please to take my advice or not, yet in one thing I was\n\
\ determined, that his majesty having frequently offered me an\n\
\ establishment in this country, I would, with great thankfulness, accept\n\
\ the favour, and pass my life here in the conversation of those superior\n\
\ beings the _struldbrugs_, if they would please to admit me.\"\n\
\ \n\
\ The gentleman to whom I addressed my discourse, because (as I have\n\
\ already observed) he spoke the language of Balnibarbi, said to me, with a\n\
\ sort of a smile which usually arises from pity to the ignorant, \"that he\n\
\ was glad of any occasion to keep me among them, and desired my permission\n\
\ to explain to the company what I had spoke.\"  He did so, and they talked\n\
\ together for some time in their own language, whereof I understood not a\n\
\ syllable, neither could I observe by their countenances, what impression\n\
\ my discourse had made on them.  After a short silence, the same person\n\
\ told me, \"that his friends and mine (so he thought fit to express\n\
\ himself) were very much pleased with the judicious remarks I had made on\n\
\ the great happiness and advantages of immortal life, and they were\n\
\ desirous to know, in a particular manner, what scheme of living I should\n\
\ have formed to myself, if it had fallen to my lot to have been born a\n\
\ _struldbrug_.\"\n\
\ \n\
\ I answered, \"it was easy to be eloquent on so copious and delightful a\n\
\ subject, especially to me, who had been often apt to amuse myself with\n\
\ visions of what I should do, if I were a king, a general, or a great\n\
\ lord: and upon this very case, I had frequently run over the whole system\n\
\ how I should employ myself, and pass the time, if I were sure to live for\n\
\ ever.\n\
\ \n\
\ \"That, if it had been my good fortune to come into the world a\n\
\ _struldbrug_, as soon as I could discover my own happiness, by\n\
\ understanding the difference between life and death, I would first\n\
\ resolve, by all arts and methods, whatsoever, to procure myself riches.\n\
\ In the pursuit of which, by thrift and management, I might reasonably\n\
\ expect, in about two hundred years, to be the wealthiest man in the\n\
\ kingdom.  In the second place, I would, from my earliest youth, apply\n\
\ myself to the study of arts and sciences, by which I should arrive in\n\
\ time to excel all others in learning.  Lastly, I would carefully record\n\
\ every action and event of consequence, that happened in the public,\n\
\ impartially draw the characters of the several successions of princes and\n\
\ great ministers of state, with my own observations on every point.  I\n\
\ would exactly set down the several changes in customs, language, fashions\n\
\ of dress, diet, and diversions.  By all which acquirements, I should be a\n\
\ living treasure of knowledge and wisdom, and certainly become the oracle\n\
\ of the nation.\n\
\ \n\
\ \"I would never marry after threescore, but live in a hospitable manner,\n\
\ yet still on the saving side.  I would entertain myself in forming and\n\
\ directing the minds of hopeful young men, by convincing them, from my own\n\
\ remembrance, experience, and observation, fortified by numerous examples,\n\
\ of the usefulness of virtue in public and private life.  But my choice\n\
\ and constant companions should be a set of my own immortal brotherhood;\n\
\ among whom, I would elect a dozen from the most ancient, down to my own\n\
\ contemporaries.  Where any of these wanted fortunes, I would provide them\n\
\ with convenient lodges round my own estate, and have some of them always\n\
\ at my table; only mingling a few of the most valuable among you mortals,\n\
\ whom length of time would harden me to lose with little or no reluctance,\n\
\ and treat your posterity after the same manner; just as a man diverts\n\
\ himself with the annual succession of pinks and tulips in his garden,\n\
\ without regretting the loss of those which withered the preceding year.\n\
\ \n\
\ \"These _struldbrugs_ and I would mutually communicate our observations\n\
\ and memorials, through the course of time; remark the several gradations\n\
\ by which corruption steals into the world, and oppose it in every step,\n\
\ by giving perpetual warning and instruction to mankind; which, added to\n\
\ the strong influence of our own example, would probably prevent that\n\
\ continual degeneracy of human nature so justly complained of in all ages.\n\
\ \n\
\ \"Add to this, the pleasure of seeing the various revolutions of states\n\
\ and empires; the changes in the lower and upper world; ancient cities in\n\
\ ruins, and obscure villages become the seats of kings; famous rivers\n\
\ lessening into shallow brooks; the ocean leaving one coast dry, and\n\
\ overwhelming another; the discovery of many countries yet unknown;\n\
\ barbarity overrunning the politest nations, and the most barbarous become\n\
\ civilized.  I should then see the discovery of the longitude, the\n\
\ perpetual motion, the universal medicine, and many other great\n\
\ inventions, brought to the utmost perfection.\n\
\ \n\
\ \"What wonderful discoveries should we make in astronomy, by outliving and\n\
\ confirming our own predictions; by observing the progress and return of\n\
\ comets, with the changes of motion in the sun, moon, and stars!\"\n\
\ \n\
\ I enlarged upon many other topics, which the natural desire of endless\n\
\ life, and sublunary happiness, could easily furnish me with.  When I had\n\
\ ended, and the sum of my discourse had been interpreted, as before, to\n\
\ the rest of the company, there was a good deal of talk among them in the\n\
\ language of the country, not without some laughter at my expense.  At\n\
\ last, the same gentleman who had been my interpreter, said, \"he was\n\
\ desired by the rest to set me right in a few mistakes, which I had fallen\n\
\ into through the common imbecility of human nature, and upon that\n\
\ allowance was less answerable for them.  That this breed of _struldbrugs_\n\
\ was peculiar to their country, for there were no such people either in\n\
\ Balnibarbi or Japan, where he had the honour to be ambassador from his\n\
\ majesty, and found the natives in both those kingdoms very hard to\n\
\ believe that the fact was possible: and it appeared from my astonishment\n\
\ when he first mentioned the matter to me, that I received it as a thing\n\
\ wholly new, and scarcely to be credited.  That in the two kingdoms above\n\
\ mentioned, where, during his residence, he had conversed very much, he\n\
\ observed long life to be the universal desire and wish of mankind.  That\n\
\ whoever had one foot in the grave was sure to hold back the other as\n\
\ strongly as he could.  That the oldest had still hopes of living one day\n\
\ longer, and looked on death as the greatest evil, from which nature\n\
\ always prompted him to retreat.  Only in this island of Luggnagg the\n\
\ appetite for living was not so eager, from the continual example of the\n\
\ _struldbrugs_ before their eyes.\n\
\ \n\
\ \"That the system of living contrived by me, was unreasonable and unjust;\n\
\ because it supposed a perpetuity of youth, health, and vigour, which no\n\
\ man could be so foolish to hope, however extravagant he may be in his\n\
\ wishes.  That the question therefore was not, whether a man would choose\n\
\ to be always in the prime of youth, attended with prosperity and health;\n\
\ but how he would pass a perpetual life under all the usual disadvantages\n\
\ which old age brings along with it.  For although few men will avow their\n\
\ desires of being immortal, upon such hard conditions, yet in the two\n\
\ kingdoms before mentioned, of Balnibarbi and Japan, he observed that\n\
\ every man desired to put off death some time longer, let it approach ever\n\
\ so late: and he rarely heard of any man who died willingly, except he\n\
\ were incited by the extremity of grief or torture.  And he appealed to\n\
\ me, whether in those countries I had travelled, as well as my own, I had\n\
\ not observed the same general disposition.\"\n\
\ \n\
\ After this preface, he gave me a particular account of the _struldbrugs_\n\
\ among them.  He said, \"they commonly acted like mortals till about thirty\n\
\ years old; after which, by degrees, they grew melancholy and dejected,\n\
\ increasing in both till they came to fourscore.  This he learned from\n\
\ their own confession: for otherwise, there not being above two or three\n\
\ of that species born in an age, they were too few to form a general\n\
\ observation by.  When they came to fourscore years, which is reckoned the\n\
\ extremity of living in this country, they had not only all the follies\n\
\ and infirmities of other old men, but many more which arose from the\n\
\ dreadful prospect of never dying.  They were not only opinionative,\n\
\ peevish, covetous, morose, vain, talkative, but incapable of friendship,\n\
\ and dead to all natural affection, which never descended below their\n\
\ grandchildren.  Envy and impotent desires are their prevailing passions.\n\
\ But those objects against which their envy seems principally directed,\n\
\ are the vices of the younger sort and the deaths of the old.  By\n\
\ reflecting on the former, they find themselves cut off from all\n\
\ possibility of pleasure; and whenever they see a funeral, they lament and\n\
\ repine that others have gone to a harbour of rest to which they\n\
\ themselves never can hope to arrive.  They have no remembrance of\n\
\ anything but what they learned and observed in their youth and\n\
\ middle-age, and even that is very imperfect; and for the truth or\n\
\ particulars of any fact, it is safer to depend on common tradition, than\n\
\ upon their best recollections.  The least miserable among them appear to\n\
\ be those who turn to dotage, and entirely lose their memories; these meet\n\
\ with more pity and assistance, because they want many bad qualities which\n\
\ abound in others.\n\
\ \n\
\ \"If a _struldbrug_ happen to marry one of his own kind, the marriage is\n\
\ dissolved of course, by the courtesy of the kingdom, as soon as the\n\
\ younger of the two comes to be fourscore; for the law thinks it a\n\
\ reasonable indulgence, that those who are condemned, without any fault of\n\
\ their own, to a perpetual continuance in the world, should not have their\n\
\ misery doubled by the load of a wife.\n\
\ \n\
\ \"As soon as they have completed the term of eighty years, they are looked\n\
\ on as dead in law; their heirs immediately succeed to their estates; only\n\
\ a small pittance is reserved for their support; and the poor ones are\n\
\ maintained at the public charge.  After that period, they are held\n\
\ incapable of any employment of trust or profit; they cannot purchase\n\
\ lands, or take leases; neither are they allowed to be witnesses in any\n\
\ cause, either civil or criminal, not even for the decision of meers and\n\
\ bounds.\n\
\ \n\
\ \"At ninety, they lose their teeth and hair; they have at that age no\n\
\ distinction of taste, but eat and drink whatever they can get, without\n\
\ relish or appetite.  The diseases they were subject to still continue,\n\
\ without increasing or diminishing.  In talking, they forget the common\n\
\ appellation of things, and the names of persons, even of those who are\n\
\ their nearest friends and relations.  For the same reason, they never can\n\
\ amuse themselves with reading, because their memory will not serve to\n\
\ carry them from the beginning of a sentence to the end; and by this\n\
\ defect, they are deprived of the only entertainment whereof they might\n\
\ otherwise be capable.\n\
\ \n\
\ \"The language of this country being always upon the flux, the\n\
\ _struldbrugs_ of one age do not understand those of another; neither are\n\
\ they able, after two hundred years, to hold any conversation (farther\n\
\ than by a few general words) with their neighbours the mortals; and thus\n\
\ they lie under the disadvantage of living like foreigners in their own\n\
\ country.\"\n\
\ \n\
\ This was the account given me of the _struldbrugs_, as near as I can\n\
\ remember.  I afterwards saw five or six of different ages, the youngest\n\
\ not above two hundred years old, who were brought to me at several times\n\
\ by some of my friends; but although they were told, \"that I was a great\n\
\ traveller, and had seen all the world,\" they had not the least curiosity\n\
\ to ask me a question; only desired \"I would give them _slumskudask_,\" or\n\
\ a token of remembrance; which is a modest way of begging, to avoid the\n\
\ law, that strictly forbids it, because they are provided for by the\n\
\ public, although indeed with a very scanty allowance.\n\
\ \n\
\ They are despised and hated by all sorts of people.  When one of them is\n\
\ born, it is reckoned ominous, and their birth is recorded very\n\
\ particularly so that you may know their age by consulting the register,\n\
\ which, however, has not been kept above a thousand years past, or at\n\
\ least has been destroyed by time or public disturbances.  But the usual\n\
\ way of computing how old they are, is by asking them what kings or great\n\
\ persons they can remember, and then consulting history; for infallibly\n\
\ the last prince in their mind did not begin his reign after they were\n\
\ fourscore years old.\n\
\ \n\
\ They were the most mortifying sight I ever beheld; and the women more\n\
\ horrible than the men.  Besides the usual deformities in extreme old age,\n\
\ they acquired an additional ghastliness, in proportion to their number of\n\
\ years, which is not to be described; and among half a dozen, I soon\n\
\ distinguished which was the eldest, although there was not above a\n\
\ century or two between them.\n\
\ \n\
\ The reader will easily believe, that from what I had hear and seen, my\n\
\ keen appetite for perpetuity of life was much abated.  I grew heartily\n\
\ ashamed of the pleasing visions I had formed; and thought no tyrant could\n\
\ invent a death into which I would not run with pleasure, from such a\n\
\ life.  The king heard of all that had passed between me and my friends\n\
\ upon this occasion, and rallied me very pleasantly; wishing I could send\n\
\ a couple of _struldbrugs_ to my own country, to arm our people against\n\
\ the fear of death; but this, it seems, is forbidden by the fundamental\n\
\ laws of the kingdom, or else I should have been well content with the\n\
\ trouble and expense of transporting them.\n\
\ \n\
\ I could not but agree, that the laws of this kingdom relative to the\n\
\ _struldbrugs_ were founded upon the strongest reasons, and such as any\n\
\ other country would be under the necessity of enacting, in the like\n\
\ circumstances.  Otherwise, as avarice is the necessary consequence of old\n\
\ age, those immortals would in time become proprietors of the whole\n\
\ nation, and engross the civil power, which, for want of abilities to\n\
\ manage, must end in the ruin of the public.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER XI.\n\
\ \n\
\ \n\
\ The author leaves Luggnagg, and sails to Japan.  From thence he returns\n\
\ in a Dutch ship to Amsterdam, and from Amsterdam to England.\n\
\ \n\
\ I thought this account of the _struldbrugs_ might be some entertainment\n\
\ to the reader, because it seems to be a little out of the common way; at\n\
\ least I do not remember to have met the like in any book of travels that\n\
\ has come to my hands: and if I am deceived, my excuse must be, that it is\n\
\ necessary for travellers who describe the same country, very often to\n\
\ agree in dwelling on the same particulars, without deserving the censure\n\
\ of having borrowed or transcribed from those who wrote before them.\n\
\ \n\
\ There is indeed a perpetual commerce between this kingdom and the great\n\
\ empire of Japan; and it is very probable, that the Japanese authors may\n\
\ have given some account of the _struldbrugs_; but my stay in Japan was so\n\
\ short, and I was so entirely a stranger to the language, that I was not\n\
\ qualified to make any inquiries.  But I hope the Dutch, upon this notice,\n\
\ will be curious and able enough to supply my defects.\n\
\ \n\
\ His majesty having often pressed me to accept some employment in his\n\
\ court, and finding me absolutely determined to return to my native\n\
\ country, was pleased to give me his license to depart; and honoured me\n\
\ with a letter of recommendation, under his own hand, to the Emperor of\n\
\ Japan.  He likewise presented me with four hundred and forty-four large\n\
\ pieces of gold (this nation delighting in even numbers), and a red\n\
\ diamond, which I sold in England for eleven hundred pounds.\n\
\ \n\
\ On the 6th of May, 1709, I took a solemn leave of his majesty, and all my\n\
\ friends.  This prince was so gracious as to order a guard to conduct me\n\
\ to Glanguenstald, which is a royal port to the south-west part of the\n\
\ island.  In six days I found a vessel ready to carry me to Japan, and\n\
\ spent fifteen days in the voyage.  We landed at a small port-town called\n\
\ Xamoschi, situated on the south-east part of Japan; the town lies on the\n\
\ western point, where there is a narrow strait leading northward into\n\
\ along arm of the sea, upon the north-west part of which, Yedo, the\n\
\ metropolis, stands.  At landing, I showed the custom-house officers my\n\
\ letter from the king of Luggnagg to his imperial majesty.  They knew the\n\
\ seal perfectly well; it was as broad as the palm of my hand.  The\n\
\ impression was, _A king lifting up a lame beggar from the earth_.  The\n\
\ magistrates of the town, hearing of my letter, received me as a public\n\
\ minister.  They provided me with carriages and servants, and bore my\n\
\ charges to Yedo; where I was admitted to an audience, and delivered my\n\
\ letter, which was opened with great ceremony, and explained to the\n\
\ Emperor by an interpreter, who then gave me notice, by his majesty's\n\
\ order, \"that I should signify my request, and, whatever it were, it\n\
\ should be granted, for the sake of his royal brother of Luggnagg.\"  This\n\
\ interpreter was a person employed to transact affairs with the\n\
\ Hollanders.  He soon conjectured, by my countenance, that I was a\n\
\ European, and therefore repeated his majesty's commands in Low Dutch,\n\
\ which he spoke perfectly well.  I answered, as I had before determined,\n\
\ \"that I was a Dutch merchant, shipwrecked in a very remote country,\n\
\ whence I had travelled by sea and land to Luggnagg, and then took\n\
\ shipping for Japan; where I knew my countrymen often traded, and with\n\
\ some of these I hoped to get an opportunity of returning into Europe: I\n\
\ therefore most humbly entreated his royal favour, to give order that I\n\
\ should be conducted in safety to Nangasac.\"  To this I added another\n\
\ petition, \"that for the sake of my patron the king of Luggnagg, his\n\
\ majesty would condescend to excuse my performing the ceremony imposed on\n\
\ my countrymen, of trampling upon the crucifix: because I had been thrown\n\
\ into his kingdom by my misfortunes, without any intention of trading.\"\n\
\ When this latter petition was interpreted to the Emperor, he seemed a\n\
\ little surprised; and said, \"he believed I was the first of my countrymen\n\
\ who ever made any scruple in this point; and that he began to doubt,\n\
\ whether I was a real Hollander, or not; but rather suspected I must be a\n\
\ Christian.  However, for the reasons I had offered, but chiefly to\n\
\ gratify the king of Luggnagg by an uncommon mark of his favour, he would\n\
\ comply with the singularity of my humour; but the affair must be managed\n\
\ with dexterity, and his officers should be commanded to let me pass, as\n\
\ it were by forgetfulness.  For he assured me, that if the secret should\n\
\ be discovered by my countrymen the Dutch, they would cut my throat in the\n\
\ voyage.\"  I returned my thanks, by the interpreter, for so unusual a\n\
\ favour; and some troops being at that time on their march to Nangasac,\n\
\ the commanding officer had orders to convey me safe thither, with\n\
\ particular instructions about the business of the crucifix.\n\
\ \n\
\ On the 9th day of June, 1709, I arrived at Nangasac, after a very long\n\
\ and troublesome journey.  I soon fell into the company of some Dutch\n\
\ sailors belonging to the Amboyna, of Amsterdam, a stout ship of 450 tons.\n\
\ I had lived long in Holland, pursuing my studies at Leyden, and I spoke\n\
\ Dutch well.  The seamen soon knew whence I came last: they were curious\n\
\ to inquire into my voyages and course of life.  I made up a story as\n\
\ short and probable as I could, but concealed the greatest part.  I knew\n\
\ many persons in Holland.  I was able to invent names for my parents, whom\n\
\ I pretended to be obscure people in the province of Gelderland.  I would\n\
\ have given the captain (one Theodorus Vangrult) what he pleased to ask\n\
\ for my voyage to Holland; but understanding I was a surgeon, he was\n\
\ contented to take half the usual rate, on condition that I would serve\n\
\ him in the way of my calling.  Before we took shipping, I was often asked\n\
\ by some of the crew, whether I had performed the ceremony above\n\
\ mentioned?  I evaded the question by general answers; \"that I had\n\
\ satisfied the Emperor and court in all particulars.\"  However, a\n\
\ malicious rogue of a skipper went to an officer, and pointing to me, told\n\
\ him, \"I had not yet trampled on the crucifix;\" but the other, who had\n\
\ received instructions to let me pass, gave the rascal twenty strokes on\n\
\ the shoulders with a bamboo; after which I was no more troubled with such\n\
\ questions.\n\
\ \n\
\ Nothing happened worth mentioning in this voyage.  We sailed with a fair\n\
\ wind to the Cape of Good Hope, where we staid only to take in fresh\n\
\ water.  On the 10th of April, 1710, we arrived safe at Amsterdam, having\n\
\ lost only three men by sickness in the voyage, and a fourth, who fell\n\
\ from the foremast into the sea, not far from the coast of Guinea.  From\n\
\ Amsterdam I soon after set sail for England, in a small vessel belonging\n\
\ to that city.\n\
\ \n\
\ On the 16th of April we put in at the Downs.  I landed next morning, and\n\
\ saw once more my native country, after an absence of five years and six\n\
\ months complete.  I went straight to Redriff, where I arrived the same\n\
\ day at two in the afternoon, and found my wife and family in good health.\n\
\ \n\
\ \n\
\ \n\
\ \n\
\ \n\
\ \n\
\ PART IV.  A VOYAGE TO THE COUNTRY OF THE HOUYHNHNMS.\n\
\ \n\
\ \n\
\ \n\
\ \n\
\ \n\
\ \n\
\ CHAPTER I.\n\
\ \n\
\ \n\
\ The author sets out as captain of a ship.  His men conspire against him,\n\
\ confine him a long time to his cabin, and set him on shore in an unknown\n\
\ land.  He travels up into the country.  The Yahoos, a strange sort of\n\
\ animal, described.  The author meets two Houyhnhnms.\n\
\ \n\
\ I continued at home with my wife and children about five months, in a\n\
\ very happy condition, if I could have learned the lesson of knowing when\n\
\ I was well.  I left my poor wife big with child, and accepted an\n\
\ advantageous offer made me to be captain of the Adventurer, a stout\n\
\ merchantman of 350 tons: for I understood navigation well, and being\n\
\ grown weary of a surgeon's employment at sea, which, however, I could\n\
\ exercise upon occasion, I took a skilful young man of that calling, one\n\
\ Robert Purefoy, into my ship.  We set sail from Portsmouth upon the 7th\n\
\ day of September, 1710; on the 14th we met with Captain Pocock, of\n\
\ Bristol, at Teneriffe, who was going to the bay of Campechy to cut\n\
\ logwood.  On the 16th, he was parted from us by a storm; I heard since my\n\
\ return, that his ship foundered, and none escaped but one cabin boy.  He\n\
\ was an honest man, and a good sailor, but a little too positive in his\n\
\ own opinions, which was the cause of his destruction, as it has been with\n\
\ several others; for if he had followed my advice, he might have been safe\n\
\ at home with his family at this time, as well as myself.\n\
\ \n\
\ I had several men who died in my ship of calentures, so that I was forced\n\
\ to get recruits out of Barbadoes and the Leeward Islands, where I\n\
\ touched, by the direction of the merchants who employed me; which I had\n\
\ soon too much cause to repent: for I found afterwards, that most of them\n\
\ had been buccaneers.  I had fifty hands onboard; and my orders were, that\n\
\ I should trade with the Indians in the South-Sea, and make what\n\
\ discoveries I could.  These rogues, whom I had picked up, debauched my\n\
\ other men, and they all formed a conspiracy to seize the ship, and secure\n\
\ me; which they did one morning, rushing into my cabin, and binding me\n\
\ hand and foot, threatening to throw me overboard, if I offered to stir.\n\
\ I told them, \"I was their prisoner, and would submit.\"  This they made me\n\
\ swear to do, and then they unbound me, only fastening one of my legs with\n\
\ a chain, near my bed, and placed a sentry at my door with his piece\n\
\ charged, who was commanded to shoot me dead if I attempted my liberty.\n\
\ They sent me own victuals and drink, and took the government of the ship\n\
\ to themselves.  Their design was to turn pirates and, plunder the\n\
\ Spaniards, which they could not do till they got more men.  But first\n\
\ they resolved to sell the goods the ship, and then go to Madagascar for\n\
\ recruits, several among them having died since my confinement.  They\n\
\ sailed many weeks, and traded with the Indians; but I knew not what\n\
\ course they took, being kept a close prisoner in my cabin, and expecting\n\
\ nothing less than to be murdered, as they often threatened me.\n\
\ \n\
\ Upon the 9th day of May, 1711, one James Welch came down to my cabin, and\n\
\ said, \"he had orders from the captain to set me ashore.\"  I expostulated\n\
\ with him, but in vain; neither would he so much as tell me who their new\n\
\ captain was.  They forced me into the long-boat, letting me put on my\n\
\ best suit of clothes, which were as good as new, and take a small bundle\n\
\ of linen, but no arms, except my hanger; and they were so civil as not to\n\
\ search my pockets, into which I conveyed what money I had, with some\n\
\ other little necessaries.  They rowed about a league, and then set me\n\
\ down on a strand.  I desired them to tell me what country it was.  They\n\
\ all swore, \"they knew no more than myself;\" but said, \"that the captain\"\n\
\ (as they called him) \"was resolved, after they had sold the lading, to\n\
\ get rid of me in the first place where they could discover land.\"  They\n\
\ pushed off immediately, advising me to make haste for fear of being\n\
\ overtaken by the tide, and so bade me farewell.\n\
\ \n\
\ In this desolate condition I advanced forward, and soon got upon firm\n\
\ ground, where I sat down on a bank to rest myself, and consider what I\n\
\ had best do.  When I was a little refreshed, I went up into the country,\n\
\ resolving to deliver myself to the first savages I should meet, and\n\
\ purchase my life from them by some bracelets, glass rings, and other\n\
\ toys, which sailors usually provide themselves with in those voyages, and\n\
\ whereof I had some about me.  The land was divided by long rows of trees,\n\
\ not regularly planted, but naturally growing; there was great plenty of\n\
\ grass, and several fields of oats.  I walked very circumspectly, for fear\n\
\ of being surprised, or suddenly shot with an arrow from behind, or on\n\
\ either side.  I fell into a beaten road, where I saw many tracts of human\n\
\ feet, and some of cows, but most of horses.  At last I beheld several\n\
\ animals in a field, and one or two of the same kind sitting in trees.\n\
\ Their shape was very singular and deformed, which a little discomposed\n\
\ me, so that I lay down behind a thicket to observe them better.  Some of\n\
\ them coming forward near the place where I lay, gave me an opportunity of\n\
\ distinctly marking their form.  Their heads and breasts were covered with\n\
\ a thick hair, some frizzled, and others lank; they had beards like goats,\n\
\ and a long ridge of hair down their backs, and the fore parts of their\n\
\ legs and feet; but the rest of their bodies was bare, so that I might see\n\
\ their skins, which were of a brown buff colour.  They had no tails, nor\n\
\ any hair at all on their buttocks, except about the anus, which, I\n\
\ presume, nature had placed there to defend them as they sat on the\n\
\ ground, for this posture they used, as well as lying down, and often\n\
\ stood on their hind feet.  They climbed high trees as nimbly as a\n\
\ squirrel, for they had strong extended claws before and behind,\n\
\ terminating in sharp points, and hooked.  They would often spring, and\n\
\ bound, and leap, with prodigious agility.  The females were not so large\n\
\ as the males; they had long lank hair on their heads, but none on their\n\
\ faces, nor any thing more than a sort of down on the rest of their\n\
\ bodies, except about the anus and pudenda.  The dugs hung between their\n\
\ fore feet, and often reached almost to the ground as they walked.  The\n\
\ hair of both sexes was of several colours, brown, red, black, and yellow.\n\
\ Upon the whole, I never beheld, in all my travels, so disagreeable an\n\
\ animal, or one against which I naturally conceived so strong an\n\
\ antipathy.  So that, thinking I had seen enough, full of contempt and\n\
\ aversion, I got up, and pursued the beaten road, hoping it might direct\n\
\ me to the cabin of some Indian.  I had not got far, when I met one of\n\
\ these creatures full in my way, and coming up directly to me.  The ugly\n\
\ monster, when he saw me, distorted several ways, every feature of his\n\
\ visage, and stared, as at an object he had never seen before; then\n\
\ approaching nearer, lifted up his fore-paw, whether out of curiosity or\n\
\ mischief I could not tell; but I drew my hanger, and gave him a good blow\n\
\ with the flat side of it, for I durst not strike with the edge, fearing\n\
\ the inhabitants might be provoked against me, if they should come to know\n\
\ that I had killed or maimed any of their cattle.  When the beast felt the\n\
\ smart, he drew back, and roared so loud, that a herd of at least forty\n\
\ came flocking about me from the next field, howling and making odious\n\
\ faces; but I ran to the body of a tree, and leaning my back against it,\n\
\ kept them off by waving my hanger.  Several of this cursed brood, getting\n\
\ hold of the branches behind, leaped up into the tree, whence they began\n\
\ to discharge their excrements on my head; however, I escaped pretty well\n\
\ by sticking close to the stem of the tree, but was almost stifled with\n\
\ the filth, which fell about me on every side.\n\
\ \n\
\ In the midst of this distress, I observed them all to run away on a\n\
\ sudden as fast as they could; at which I ventured to leave the tree and\n\
\ pursue the road, wondering what it was that could put them into this\n\
\ fright.  But looking on my left hand, I saw a horse walking softly in the\n\
\ field; which my persecutors having sooner discovered, was the cause of\n\
\ their flight.  The horse started a little, when he came near me, but soon\n\
\ recovering himself, looked full in my face with manifest tokens of\n\
\ wonder; he viewed my hands and feet, walking round me several times.  I\n\
\ would have pursued my journey, but he placed himself directly in the way,\n\
\ yet looking with a very mild aspect, never offering the least violence.\n\
\ We stood gazing at each other for some time; at last I took the boldness\n\
\ to reach my hand towards his neck with a design to stroke it, using the\n\
\ common style and whistle of jockeys, when they are going to handle a\n\
\ strange horse.  But this animal seemed to receive my civilities with\n\
\ disdain, shook his head, and bent his brows, softly raising up his right\n\
\ fore-foot to remove my hand.  Then he neighed three or four times, but in\n\
\ so different a cadence, that I almost began to think he was speaking to\n\
\ himself, in some language of his own.\n\
\ \n\
\ While he and I were thus employed, another horse came up; who applying\n\
\ himself to the first in a very formal manner, they gently struck each\n\
\ other's right hoof before, neighing several times by turns, and varying\n\
\ the sound, which seemed to be almost articulate.  They went some paces\n\
\ off, as if it were to confer together, walking side by side, backward and\n\
\ forward, like persons deliberating upon some affair of weight, but often\n\
\ turning their eyes towards me, as it were to watch that I might not\n\
\ escape.  I was amazed to see such actions and behaviour in brute beasts;\n\
\ and concluded with myself, that if the inhabitants of this country were\n\
\ endued with a proportionable degree of reason, they must needs be the\n\
\ wisest people upon earth.  This thought gave me so much comfort, that I\n\
\ resolved to go forward, until I could discover some house or village, or\n\
\ meet with any of the natives, leaving the two horses to discourse\n\
\ together as they pleased.  But the first, who was a dapple gray,\n\
\ observing me to steal off, neighed after me in so expressive a tone, that\n\
\ I fancied myself to understand what he meant; whereupon I turned back,\n\
\ and came near to him to expect his farther commands: but concealing my\n\
\ fear as much as I could, for I began to be in some pain how this\n\
\ adventure might terminate; and the reader will easily believe I did not\n\
\ much like my present situation.\n\
\ \n\
\ The two horses came up close to me, looking with great earnestness upon\n\
\ my face and hands.  The gray steed rubbed my hat all round with his right\n\
\ fore-hoof, and discomposed it so much that I was forced to adjust it\n\
\ better by taking it off and settling it again; whereat, both he and his\n\
\ companion (who was a brown bay) appeared to be much surprised: the latter\n\
\ felt the lappet of my coat, and finding it to hang loose about me, they\n\
\ both looked with new signs of wonder.  He stroked my right hand, seeming\n\
\ to admire the softness and colour; but he squeezed it so hard between his\n\
\ hoof and his pastern, that I was forced to roar; after which they both\n\
\ touched me with all possible tenderness.  They were under great\n\
\ perplexity about my shoes and stockings, which they felt very often,\n\
\ neighing to each other, and using various gestures, not unlike those of a\n\
\ philosopher, when he would attempt to solve some new and difficult\n\
\ phenomenon.\n\
\ \n\
\ Upon the whole, the behaviour of these animals was so orderly and\n\
\ rational, so acute and judicious, that I at last concluded they must\n\
\ needs be magicians, who had thus metamorphosed themselves upon some\n\
\ design, and seeing a stranger in the way, resolved to divert themselves\n\
\ with him; or, perhaps, were really amazed at the sight of a man so very\n\
\ different in habit, feature, and complexion, from those who might\n\
\ probably live in so remote a climate.  Upon the strength of this\n\
\ reasoning, I ventured to address them in the following manner:\n\
\ \"Gentlemen, if you be conjurers, as I have good cause to believe, you can\n\
\ understand my language; therefore I make bold to let your worships know\n\
\ that I am a poor distressed Englishman, driven by his misfortunes upon\n\
\ your coast; and I entreat one of you to let me ride upon his back, as if\n\
\ he were a real horse, to some house or village where I can be relieved.\n\
\ In return of which favour, I will make you a present of this knife and\n\
\ bracelet,\" taking them out of my pocket.  The two creatures stood silent\n\
\ while I spoke, seeming to listen with great attention, and when I had\n\
\ ended, they neighed frequently towards each other, as if they were\n\
\ engaged in serious conversation.  I plainly observed that their language\n\
\ expressed the passions very well, and the words might, with little pains,\n\
\ be resolved into an alphabet more easily than the Chinese.\n\
\ \n\
\ I could frequently distinguish the word _Yahoo_, which was repeated by\n\
\ each of them several times: and although it was impossible for me to\n\
\ conjecture what it meant, yet while the two horses were busy in\n\
\ conversation, I endeavoured to practise this word upon my tongue; and as\n\
\ soon as they were silent, I boldly pronounced _Yahoo_ in a loud voice,\n\
\ imitating at the same time, as near as I could, the neighing of a horse;\n\
\ at which they were both visibly surprised; and the gray repeated the same\n\
\ word twice, as if he meant to teach me the right accent; wherein I spoke\n\
\ after him as well as I could, and found myself perceivably to improve\n\
\ every time, though very far from any degree of perfection.  Then the bay\n\
\ tried me with a second word, much harder to be pronounced; but reducing\n\
\ it to the English orthography, may be spelt thus, _Houyhnhnm_.  I did not\n\
\ succeed in this so well as in the former; but after two or three farther\n\
\ trials, I had better fortune; and they both appeared amazed at my\n\
\ capacity.\n\
\ \n\
\ After some further discourse, which I then conjectured might relate to\n\
\ me, the two friends took their leaves, with the same compliment of\n\
\ striking each other's hoof; and the gray made me signs that I should walk\n\
\ before him; wherein I thought it prudent to comply, till I could find a\n\
\ better director.  When I offered to slacken my pace, he would cry _hhuun\n\
\ hhuun_: I guessed his meaning, and gave him to understand, as well as I\n\
\ could, \"that I was weary, and not able to walk faster;\" upon which he\n\
\ would stand awhile to let me rest.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER II.\n\
\ \n\
\ \n\
\ The author conducted by a Houyhnhnm to his house.  The house described.\n\
\ The author's reception.  The food of the Houyhnhnms.  The author in\n\
\ distress for want of meat.  Is at last relieved.  His manner of feeding\n\
\ in this country.\n\
\ \n\
\ Having travelled about three miles, we came to a long kind of building,\n\
\ made of timber stuck in the ground, and wattled across; the roof was low\n\
\ and covered with straw.  I now began to be a little comforted; and took\n\
\ out some toys, which travellers usually carry for presents to the savage\n\
\ Indians of America, and other parts, in hopes the people of the house\n\
\ would be thereby encouraged to receive me kindly.  The horse made me a\n\
\ sign to go in first; it was a large room with a smooth clay floor, and a\n\
\ rack and manger, extending the whole length on one side.  There were\n\
\ three nags and two mares, not eating, but some of them sitting down upon\n\
\ their hams, which I very much wondered at; but wondered more to see the\n\
\ rest employed in domestic business; these seemed but ordinary cattle.\n\
\ However, this confirmed my first opinion, that a people who could so far\n\
\ civilise brute animals, must needs excel in wisdom all the nations of the\n\
\ world.  The gray came in just after, and thereby prevented any ill\n\
\ treatment which the others might have given me.  He neighed to them\n\
\ several times in a style of authority, and received answers.\n\
\ \n\
\ Beyond this room there were three others, reaching the length of the\n\
\ house, to which you passed through three doors, opposite to each other,\n\
\ in the manner of a vista.  We went through the second room towards the\n\
\ third.  Here the gray walked in first, beckoning me to attend: I waited\n\
\ in the second room, and got ready my presents for the master and mistress\n\
\ of the house; they were two knives, three bracelets of false pearls, a\n\
\ small looking-glass, and a bead necklace.  The horse neighed three or\n\
\ four times, and I waited to hear some answers in a human voice, but I\n\
\ heard no other returns than in the same dialect, only one or two a little\n\
\ shriller than his.  I began to think that this house must belong to some\n\
\ person of great note among them, because there appeared so much ceremony\n\
\ before I could gain admittance.  But, that a man of quality should be\n\
\ served all by horses, was beyond my comprehension.  I feared my brain was\n\
\ disturbed by my sufferings and misfortunes.  I roused myself, and looked\n\
\ about me in the room where I was left alone: this was furnished like the\n\
\ first, only after a more elegant manner.  I rubbed my eyes often, but the\n\
\ same objects still occurred.  I pinched my arms and sides to awake\n\
\ myself, hoping I might be in a dream.  I then absolutely concluded, that\n\
\ all these appearances could be nothing else but necromancy and magic.\n\
\ But I had no time to pursue these reflections; for the gray horse came to\n\
\ the door, and made me a sign to follow him into the third room where I\n\
\ saw a very comely mare, together with a colt and foal, sitting on their\n\
\ haunches upon mats of straw, not unartfully made, and perfectly neat and\n\
\ clean.\n\
\ \n\
\ The mare soon after my entrance rose from her mat, and coming up close,\n\
\ after having nicely observed my hands and face, gave me a most\n\
\ contemptuous look; and turning to the horse, I heard the word _Yahoo_\n\
\ often repeated betwixt them; the meaning of which word I could not then\n\
\ comprehend, although it was the first I had learned to pronounce.  But I\n\
\ was soon better informed, to my everlasting mortification; for the horse,\n\
\ beckoning to me with his head, and repeating the _hhuun_, _hhuun_, as he\n\
\ did upon the road, which I understood was to attend him, led me out into\n\
\ a kind of court, where was another building, at some distance from the\n\
\ house.  Here we entered, and I saw three of those detestable creatures,\n\
\ which I first met after my landing, feeding upon roots, and the flesh of\n\
\ some animals, which I afterwards found to be that of asses and dogs, and\n\
\ now and then a cow, dead by accident or disease.  They were all tied by\n\
\ the neck with strong withes fastened to a beam; they held their food\n\
\ between the claws of their fore feet, and tore it with their teeth.\n\
\ \n\
\ The master horse ordered a sorrel nag, one of his servants, to untie the\n\
\ largest of these animals, and take him into the yard.  The beast and I\n\
\ were brought close together, and by our countenances diligently compared\n\
\ both by master and servant, who thereupon repeated several times the word\n\
\ _Yahoo_.  My horror and astonishment are not to be described, when I\n\
\ observed in this abominable animal, a perfect human figure: the face of\n\
\ it indeed was flat and broad, the nose depressed, the lips large, and the\n\
\ mouth wide; but these differences are common to all savage nations, where\n\
\ the lineaments of the countenance are distorted, by the natives suffering\n\
\ their infants to lie grovelling on the earth, or by carrying them on\n\
\ their backs, nuzzling with their face against the mothers' shoulders.\n\
\ The fore-feet of the _Yahoo_ differed from my hands in nothing else but\n\
\ the length of the nails, the coarseness and brownness of the palms, and\n\
\ the hairiness on the backs.  There was the same resemblance between our\n\
\ feet, with the same differences; which I knew very well, though the\n\
\ horses did not, because of my shoes and stockings; the same in every part\n\
\ of our bodies except as to hairiness and colour, which I have already\n\
\ described.\n\
\ \n\
\ The great difficulty that seemed to stick with the two horses, was to see\n\
\ the rest of my body so very different from that of a _Yahoo_, for which I\n\
\ was obliged to my clothes, whereof they had no conception.  The sorrel\n\
\ nag offered me a root, which he held (after their manner, as we shall\n\
\ describe in its proper place) between his hoof and pastern; I took it in\n\
\ my hand, and, having smelt it, returned it to him again as civilly as I\n\
\ could.  He brought out of the _Yahoos_' kennel a piece of ass's flesh;\n\
\ but it smelt so offensively that I turned from it with loathing: he then\n\
\ threw it to the _Yahoo_, by whom it was greedily devoured.  He afterwards\n\
\ showed me a wisp of hay, and a fetlock full of oats; but I shook my head,\n\
\ to signify that neither of these were food for me.  And indeed I now\n\
\ apprehended that I must absolutely starve, if I did not get to some of my\n\
\ own species; for as to those filthy _Yahoos_, although there were few\n\
\ greater lovers of mankind at that time than myself, yet I confess I never\n\
\ saw any sensitive being so detestable on all accounts; and the more I\n\
\ came near them the more hateful they grew, while I stayed in that\n\
\ country.  This the master horse observed by my behaviour, and therefore\n\
\ sent the _Yahoo_ back to his kennel.  He then put his fore-hoof to his\n\
\ mouth, at which I was much surprised, although he did it with ease, and\n\
\ with a motion that appeared perfectly natural, and made other signs, to\n\
\ know what I would eat; but I could not return him such an answer as he\n\
\ was able to apprehend; and if he had understood me, I did not see how it\n\
\ was possible to contrive any way for finding myself nourishment.  While\n\
\ we were thus engaged, I observed a cow passing by, whereupon I pointed to\n\
\ her, and expressed a desire to go and milk her.  This had its effect; for\n\
\ he led me back into the house, and ordered a mare-servant to open a room,\n\
\ where a good store of milk lay in earthen and wooden vessels, after a\n\
\ very orderly and cleanly manner.  She gave me a large bowlful, of which I\n\
\ drank very heartily, and found myself well refreshed.\n\
\ \n\
\ About noon, I saw coming towards the house a kind of vehicle drawn like a\n\
\ sledge by four _Yahoos_.  There was in it an old steed, who seemed to be\n\
\ of quality; he alighted with his hind-feet forward, having by accident\n\
\ got a hurt in his left fore-foot.  He came to dine with our horse, who\n\
\ received him with great civility.  They dined in the best room, and had\n\
\ oats boiled in milk for the second course, which the old horse ate warm,\n\
\ but the rest cold.  Their mangers were placed circular in the middle of\n\
\ the room, and divided into several partitions, round which they sat on\n\
\ their haunches, upon bosses of straw.  In the middle was a large rack,\n\
\ with angles answering to every partition of the manger; so that each\n\
\ horse and mare ate their own hay, and their own mash of oats and milk,\n\
\ with much decency and regularity.  The behaviour of the young colt and\n\
\ foal appeared very modest, and that of the master and mistress extremely\n\
\ cheerful and complaisant to their guest.  The gray ordered me to stand by\n\
\ him; and much discourse passed between him and his friend concerning me,\n\
\ as I found by the stranger's often looking on me, and the frequent\n\
\ repetition of the word _Yahoo_.\n\
\ \n\
\ I happened to wear my gloves, which the master gray observing, seemed\n\
\ perplexed, discovering signs of wonder what I had done to my fore-feet.\n\
\ He put his hoof three or four times to them, as if he would signify, that\n\
\ I should reduce them to their former shape, which I presently did,\n\
\ pulling off both my gloves, and putting them into my pocket.  This\n\
\ occasioned farther talk; and I saw the company was pleased with my\n\
\ behaviour, whereof I soon found the good effects.  I was ordered to speak\n\
\ the few words I understood; and while they were at dinner, the master\n\
\ taught me the names for oats, milk, fire, water, and some others, which I\n\
\ could readily pronounce after him, having from my youth a great facility\n\
\ in learning languages.\n\
\ \n\
\ When dinner was done, the master horse took me aside, and by signs and\n\
\ words made me understand the concern he was in that I had nothing to eat.\n\
\ Oats in their tongue are called _hlunnh_.  This word I pronounced two or\n\
\ three times; for although I had refused them at first, yet, upon second\n\
\ thoughts, I considered that I could contrive to make of them a kind of\n\
\ bread, which might be sufficient, with milk, to keep me alive, till I\n\
\ could make my escape to some other country, and to creatures of my own\n\
\ species.  The horse immediately ordered a white mare servant of his\n\
\ family to bring me a good quantity of oats in a sort of wooden tray.\n\
\ These I heated before the fire, as well as I could, and rubbed them till\n\
\ the husks came off, which I made a shift to winnow from the grain.  I\n\
\ ground and beat them between two stones; then took water, and made them\n\
\ into a paste or cake, which I toasted at the fire and eat warm with milk.\n\
\ It was at first a very insipid diet, though common enough in many parts\n\
\ of Europe, but grew tolerable by time; and having been often reduced to\n\
\ hard fare in my life, this was not the first experiment I had made how\n\
\ easily nature is satisfied.  And I cannot but observe, that I never had\n\
\ one hours sickness while I stayed in this island.  It is true, I\n\
\ sometimes made a shift to catch a rabbit, or bird, by springs made of\n\
\ _Yahoo's_ hairs; and I often gathered wholesome herbs, which I boiled,\n\
\ and ate as salads with my bread; and now and then, for a rarity, I made a\n\
\ little butter, and drank the whey.  I was at first at a great loss for\n\
\ salt, but custom soon reconciled me to the want of it; and I am confident\n\
\ that the frequent use of salt among us is an effect of luxury, and was\n\
\ first introduced only as a provocative to drink, except where it is\n\
\ necessary for preserving flesh in long voyages, or in places remote from\n\
\ great markets; for we observe no animal to be fond of it but man, and as\n\
\ to myself, when I left this country, it was a great while before I could\n\
\ endure the taste of it in anything that I ate.\n\
\ \n\
\ This is enough to say upon the subject of my diet, wherewith other\n\
\ travellers fill their books, as if the readers were personally concerned\n\
\ whether we fare well or ill.  However, it was necessary to mention this\n\
\ matter, lest the world should think it impossible that I could find\n\
\ sustenance for three years in such a country, and among such inhabitants.\n\
\ \n\
\ When it grew towards evening, the master horse ordered a place for me to\n\
\ lodge in; it was but six yards from the house and separated from the\n\
\ stable of the _Yahoos_.  Here I got some straw, and covering myself with\n\
\ my own clothes, slept very sound.  But I was in a short time better\n\
\ accommodated, as the reader shall know hereafter, when I come to treat\n\
\ more particularly about my way of living.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER III.\n\
\ \n\
\ \n\
\ The author studies to learn the language.  The Houyhnhnm, his master,\n\
\ assists in teaching him.  The language described.  Several Houyhnhnms of\n\
\ quality come out of curiosity to see the author.  He gives his master a\n\
\ short account of his voyage.\n\
\ \n\
\ My principal endeavour was to learn the language, which my master (for so\n\
\ I shall henceforth call him), and his children, and every servant of his\n\
\ house, were desirous to teach me; for they looked upon it as a prodigy,\n\
\ that a brute animal should discover such marks of a rational creature.  I\n\
\ pointed to every thing, and inquired the name of it, which I wrote down\n\
\ in my journal-book when I was alone, and corrected my bad accent by\n\
\ desiring those of the family to pronounce it often.  In this employment,\n\
\ a sorrel nag, one of the under-servants, was very ready to assist me.\n\
\ \n\
\ In speaking, they pronounced through the nose and throat, and their\n\
\ language approaches nearest to the High-Dutch, or German, of any I know\n\
\ in Europe; but is much more graceful and significant.  The emperor\n\
\ Charles V. made almost the same observation, when he said \"that if he\n\
\ were to speak to his horse, it should be in High-Dutch.\"\n\
\ \n\
\ The curiosity and impatience of my master were so great, that he spent\n\
\ many hours of his leisure to instruct me.  He was convinced (as he\n\
\ afterwards told me) that I must be a _Yahoo_; but my teachableness,\n\
\ civility, and cleanliness, astonished him; which were qualities\n\
\ altogether opposite to those animals.  He was most perplexed about my\n\
\ clothes, reasoning sometimes with himself, whether they were a part of my\n\
\ body: for I never pulled them off till the family were asleep, and got\n\
\ them on before they waked in the morning.  My master was eager to learn\n\
\ \"whence I came; how I acquired those appearances of reason, which I\n\
\ discovered in all my actions; and to know my story from my own mouth,\n\
\ which he hoped he should soon do by the great proficiency I made in\n\
\ learning and pronouncing their words and sentences.\"  To help my memory,\n\
\ I formed all I learned into the English alphabet, and writ the words\n\
\ down, with the translations.  This last, after some time, I ventured to\n\
\ do in my master's presence.  It cost me much trouble to explain to him\n\
\ what I was doing; for the inhabitants have not the least idea of books or\n\
\ literature.\n\
\ \n\
\ In about ten weeks time, I was able to understand most of his questions;\n\
\ and in three months, could give him some tolerable answers.  He was\n\
\ extremely curious to know \"from what part of the country I came, and how\n\
\ I was taught to imitate a rational creature; because the _Yahoos_ (whom\n\
\ he saw I exactly resembled in my head, hands, and face, that were only\n\
\ visible), with some appearance of cunning, and the strongest disposition\n\
\ to mischief, were observed to be the most unteachable of all brutes.\"  I\n\
\ answered, \"that I came over the sea, from a far place, with many others\n\
\ of my own kind, in a great hollow vessel made of the bodies of trees:\n\
\ that my companions forced me to land on this coast, and then left me to\n\
\ shift for myself.\"  It was with some difficulty, and by the help of many\n\
\ signs, that I brought him to understand me.  He replied, \"that I must\n\
\ needs be mistaken, or that I said the thing which was not;\" for they have\n\
\ no word in their language to express lying or falsehood.  \"He knew it was\n\
\ impossible that there could be a country beyond the sea, or that a parcel\n\
\ of brutes could move a wooden vessel whither they pleased upon water.  He\n\
\ was sure no _Houyhnhnm_ alive could make such a vessel, nor would trust\n\
\ _Yahoos_ to manage it.\"\n\
\ \n\
\ The word _Houyhnhnm_, in their tongue, signifies a _horse_, and, in its\n\
\ etymology, the _perfection of nature_.  I told my master, \"that I was at\n\
\ a loss for expression, but would improve as fast as I could; and hoped,\n\
\ in a short time, I should be able to tell him wonders.\"  He was pleased\n\
\ to direct his own mare, his colt, and foal, and the servants of the\n\
\ family, to take all opportunities of instructing me; and every day, for\n\
\ two or three hours, he was at the same pains himself.  Several horses and\n\
\ mares of quality in the neighbourhood came often to our house, upon the\n\
\ report spread of \"a wonderful _Yahoo_, that could speak like a\n\
\ _Houyhnhnm_, and seemed, in his words and actions, to discover some\n\
\ glimmerings of reason.\"  These delighted to converse with me: they put\n\
\ many questions, and received such answers as I was able to return.  By\n\
\ all these advantages I made so great a progress, that, in five months\n\
\ from my arrival I understood whatever was spoken, and could express\n\
\ myself tolerably well.\n\
\ \n\
\ The _Houyhnhnms_, who came to visit my master out of a design of seeing\n\
\ and talking with me, could hardly believe me to be a right _Yahoo_,\n\
\ because my body had a different covering from others of my kind.  They\n\
\ were astonished to observe me without the usual hair or skin, except on\n\
\ my head, face, and hands; but I discovered that secret to my master upon\n\
\ an accident which happened about a fortnight before.\n\
\ \n\
\ I have already told the reader, that every night, when the family were\n\
\ gone to bed, it was my custom to strip, and cover myself with my clothes.\n\
\ It happened, one morning early, that my master sent for me by the sorrel\n\
\ nag, who was his valet.  When he came I was fast asleep, my clothes\n\
\ fallen off on one side, and my shirt above my waist.  I awaked at the\n\
\ noise he made, and observed him to deliver his message in some disorder;\n\
\ after which he went to my master, and in a great fright gave him a very\n\
\ confused account of what he had seen.  This I presently discovered, for,\n\
\ going as soon as I was dressed to pay my attendance upon his honour, he\n\
\ asked me \"the meaning of what his servant had reported, that I was not\n\
\ the same thing when I slept, as I appeared to be at other times; that his\n\
\ vale assured him, some part of me was white, some yellow, at least not so\n\
\ white, and some brown.\"\n\
\ \n\
\ I had hitherto concealed the secret of my dress, in order to distinguish\n\
\ myself, as much as possible, from that cursed race of _Yahoos_; but now I\n\
\ found it in vain to do so any longer.  Besides, I considered that my\n\
\ clothes and shoes would soon wear out, which already were in a declining\n\
\ condition, and must be supplied by some contrivance from the hides of\n\
\ _Yahoos_, or other brutes; whereby the whole secret would be known.  I\n\
\ therefore told my master, \"that in the country whence I came, those of my\n\
\ kind always covered their bodies with the hairs of certain animals\n\
\ prepared by art, as well for decency as to avoid the inclemencies of air,\n\
\ both hot and cold; of which, as to my own person, I would give him\n\
\ immediate conviction, if he pleased to command me: only desiring his\n\
\ excuse, if I did not expose those parts that nature taught us to\n\
\ conceal.\"  He said, \"my discourse was all very strange, but especially\n\
\ the last part; for he could not understand, why nature should teach us to\n\
\ conceal what nature had given; that neither himself nor family were\n\
\ ashamed of any parts of their bodies; but, however, I might do as I\n\
\ pleased.\"  Whereupon I first unbuttoned my coat, and pulled it off.  I\n\
\ did the same with my waistcoat.  I drew off my shoes, stockings, and\n\
\ breeches.  I let my shirt down to my waist, and drew up the bottom;\n\
\ fastening it like a girdle about my middle, to hide my nakedness.\n\
\ \n\
\ My master observed the whole performance with great signs of curiosity\n\
\ and admiration.  He took up all my clothes in his pastern, one piece\n\
\ after another, and examined them diligently; he then stroked my body very\n\
\ gently, and looked round me several times; after which, he said, it was\n\
\ plain I must be a perfect _Yahoo_; but that I differed very much from the\n\
\ rest of my species in the softness, whiteness, and smoothness of my skin;\n\
\ my want of hair in several parts of my body; the shape and shortness of\n\
\ my claws behind and before; and my affectation of walking continually on\n\
\ my two hinder feet.  He desired to see no more; and gave me leave to put\n\
\ on my clothes again, for I was shuddering with cold.\n\
\ \n\
\ I expressed my uneasiness at his giving me so often the appellation of\n\
\ _Yahoo_, an odious animal, for which I had so utter a hatred and\n\
\ contempt: I begged he would forbear applying that word to me, and make\n\
\ the same order in his family and among his friends whom he suffered to\n\
\ see me.  I requested likewise, \"that the secret of my having a false\n\
\ covering to my body, might be known to none but himself, at least as long\n\
\ as my present clothing should last; for as to what the sorrel nag, his\n\
\ valet, had observed, his honour might command him to conceal it.\"\n\
\ \n\
\ All this my master very graciously consented to; and thus the secret was\n\
\ kept till my clothes began to wear out, which I was forced to supply by\n\
\ several contrivances that shall hereafter be mentioned.  In the meantime,\n\
\ he desired \"I would go on with my utmost diligence to learn their\n\
\ language, because he was more astonished at my capacity for speech and\n\
\ reason, than at the figure of my body, whether it were covered or not;\"\n\
\ adding, \"that he waited with some impatience to hear the wonders which I\n\
\ promised to tell him.\"\n\
\ \n\
\ Thenceforward he doubled the pains he had been at to instruct me: he\n\
\ brought me into all company, and made them treat me with civility;\n\
\ \"because,\" as he told them, privately, \"this would put me into good\n\
\ humour, and make me more diverting.\"\n\
\ \n\
\ Every day, when I waited on him, beside the trouble he was at in\n\
\ teaching, he would ask me several questions concerning myself, which I\n\
\ answered as well as I could, and by these means he had already received\n\
\ some general ideas, though very imperfect.  It would be tedious to relate\n\
\ the several steps by which I advanced to a more regular conversation; but\n\
\ the first account I gave of myself in any order and length was to this\n\
\ purpose:\n\
\ \n\
\ \"That I came from a very far country, as I already had attempted to tell\n\
\ him, with about fifty more of my own species; that we travelled upon the\n\
\ seas in a great hollow vessel made of wood, and larger than his honour's\n\
\ house.  I described the ship to him in the best terms I could, and\n\
\ explained, by the help of my handkerchief displayed, how it was driven\n\
\ forward by the wind.  That upon a quarrel among us, I was set on shore on\n\
\ this coast, where I walked forward, without knowing whither, till he\n\
\ delivered me from the persecution of those execrable _Yahoos_.\"  He asked\n\
\ me, \"who made the ship, and how it was possible that the _Houyhnhnms_ of\n\
\ my country would leave it to the management of brutes?\"  My answer was,\n\
\ \"that I durst proceed no further in my relation, unless he would give me\n\
\ his word and honour that he would not be offended, and then I would tell\n\
\ him the wonders I had so often promised.\"  He agreed; and I went on by\n\
\ assuring him, that the ship was made by creatures like myself; who, in\n\
\ all the countries I had travelled, as well as in my own, were the only\n\
\ governing rational animals; and that upon my arrival hither, I was as\n\
\ much astonished to see the _Houyhnhnms_ act like rational beings, as he,\n\
\ or his friends, could be, in finding some marks of reason in a creature\n\
\ he was pleased to call a _Yahoo_; to which I owned my resemblance in\n\
\ every part, but could not account for their degenerate and brutal nature.\n\
\ I said farther, \"that if good fortune ever restored me to my native\n\
\ country, to relate my travels hither, as I resolved to do, everybody\n\
\ would believe, that I said the thing that was not, that I invented the\n\
\ story out of my own head; and (with all possible respect to himself, his\n\
\ family, and friends, and under his promise of not being offended) our\n\
\ countrymen would hardly think it probable that a _Houyhnhnm_ should be\n\
\ the presiding creature of a nation, and a _Yahoo_ the brute.\"\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER IV.\n\
\ \n\
\ \n\
\ The Houyhnhnm's notion of truth and falsehood.  The author's discourse\n\
\ disapproved by his master.  The author gives a more particular account of\n\
\ himself, and the accidents of his voyage.\n\
\ \n\
\ My master heard me with great appearances of uneasiness in his\n\
\ countenance; because doubting, or not believing, are so little known in\n\
\ this country, that the inhabitants cannot tell how to behave themselves\n\
\ under such circumstances.  And I remember, in frequent discourses with my\n\
\ master concerning the nature of manhood in other parts of the world,\n\
\ having occasion to talk of lying and false representation, it was with\n\
\ much difficulty that he comprehended what I meant, although he had\n\
\ otherwise a most acute judgment.  For he argued thus: \"that the use of\n\
\ speech was to make us understand one another, and to receive information\n\
\ of facts; now, if any one said the thing which was not, these ends were\n\
\ defeated, because I cannot properly be said to understand him; and I am\n\
\ so far from receiving information, that he leaves me worse than in\n\
\ ignorance; for I am led to believe a thing black, when it is white, and\n\
\ short, when it is long.\"  And these were all the notions he had\n\
\ concerning that faculty of lying, so perfectly well understood, and so\n\
\ universally practised, among human creatures.\n\
\ \n\
\ To return from this digression.  When I asserted that the _Yahoos_ were\n\
\ the only governing animals in my country, which my master said was\n\
\ altogether past his conception, he desired to know, \"whether we had\n\
\ _Houyhnhnms_ among us, and what was their employment?\"  I told him, \"we\n\
\ had great numbers; that in summer they grazed in the fields, and in\n\
\ winter were kept in houses with hay and oats, where _Yahoo_ servants were\n\
\ employed to rub their skins smooth, comb their manes, pick their feet,\n\
\ serve them with food, and make their beds.\"  \"I understand you well,\"\n\
\ said my master: \"it is now very plain, from all you have spoken, that\n\
\ whatever share of reason the _Yahoos_ pretend to, the _Houyhnhnms_ are\n\
\ your masters; I heartily wish our _Yahoos_ would be so tractable.\"  I\n\
\ begged \"his honour would please to excuse me from proceeding any further,\n\
\ because I was very certain that the account he expected from me would be\n\
\ highly displeasing.\"  But he insisted in commanding me to let him know\n\
\ the best and the worst.  I told him \"he should be obeyed.\"  I owned \"that\n\
\ the _Houyhnhnms_ among us, whom we called horses, were the most generous\n\
\ and comely animals we had; that they excelled in strength and swiftness;\n\
\ and when they belonged to persons of quality, were employed in\n\
\ travelling, racing, or drawing chariots; they were treated with much\n\
\ kindness and care, till they fell into diseases, or became foundered in\n\
\ the feet; but then they were sold, and used to all kind of drudgery till\n\
\ they died; after which their skins were stripped, and sold for what they\n\
\ were worth, and their bodies left to be devoured by dogs and birds of\n\
\ prey.  But the common race of horses had not so good fortune, being kept\n\
\ by farmers and carriers, and other mean people, who put them to greater\n\
\ labour, and fed them worse.\"  I described, as well as I could, our way of\n\
\ riding; the shape and use of a bridle, a saddle, a spur, and a whip; of\n\
\ harness and wheels.  I added, \"that we fastened plates of a certain hard\n\
\ substance, called iron, at the bottom of their feet, to preserve their\n\
\ hoofs from being broken by the stony ways, on which we often travelled.\"\n\
\ \n\
\ My master, after some expressions of great indignation, wondered \"how we\n\
\ dared to venture upon a _Houyhnhnm's_ back; for he was sure, that the\n\
\ weakest servant in his house would be able to shake off the strongest\n\
\ _Yahoo_; or by lying down and rolling on his back, squeeze the brute to\n\
\ death.\"  I answered \"that our horses were trained up, from three or four\n\
\ years old, to the several uses we intended them for; that if any of them\n\
\ proved intolerably vicious, they were employed for carriages; that they\n\
\ were severely beaten, while they were young, for any mischievous tricks;\n\
\ that the males, designed for the common use of riding or draught, were\n\
\ generally castrated about two years after their birth, to take down their\n\
\ spirits, and make them more tame and gentle; that they were indeed\n\
\ sensible of rewards and punishments; but his honour would please to\n\
\ consider, that they had not the least tincture of reason, any more than\n\
\ the _Yahoos_ in this country.\"\n\
\ \n\
\ It put me to the pains of many circumlocutions, to give my master a right\n\
\ idea of what I spoke; for their language does not abound in variety of\n\
\ words, because their wants and passions are fewer than among us.  But it\n\
\ is impossible to express his noble resentment at our savage treatment of\n\
\ the _Houyhnhnm_ race; particularly after I had explained the manner and\n\
\ use of castrating horses among us, to hinder them from propagating their\n\
\ kind, and to render them more servile.  He said, \"if it were possible\n\
\ there could be any country where _Yahoos_ alone were endued with reason,\n\
\ they certainly must be the governing animal; because reason in time will\n\
\ always prevail against brutal strength.  But, considering the frame of\n\
\ our bodies, and especially of mine, he thought no creature of equal bulk\n\
\ was so ill-contrived for employing that reason in the common offices of\n\
\ life;\" whereupon he desired to know \"whether those among whom I lived\n\
\ resembled me, or the _Yahoos_ of his country?\"  I assured him, \"that I\n\
\ was as well shaped as most of my age; but the younger, and the females,\n\
\ were much more soft and tender, and the skins of the latter generally as\n\
\ white as milk.\"  He said, \"I differed indeed from other _Yahoos_, being\n\
\ much more cleanly, and not altogether so deformed; but, in point of real\n\
\ advantage, he thought I differed for the worse: that my nails were of no\n\
\ use either to my fore or hinder feet; as to my fore feet, he could not\n\
\ properly call them by that name, for he never observed me to walk upon\n\
\ them; that they were too soft to bear the ground; that I generally went\n\
\ with them uncovered; neither was the covering I sometimes wore on them of\n\
\ the same shape, or so strong as that on my feet behind: that I could not\n\
\ walk with any security, for if either of my hinder feet slipped, I must\n\
\ inevitably fail.\"  He then began to find fault with other parts of my\n\
\ body: \"the flatness of my face, the prominence of my nose, mine eyes\n\
\ placed directly in front, so that I could not look on either side without\n\
\ turning my head: that I was not able to feed myself, without lifting one\n\
\ of my fore-feet to my mouth: and therefore nature had placed those joints\n\
\ to answer that necessity.  He knew not what could be the use of those\n\
\ several clefts and divisions in my feet behind; that these were too soft\n\
\ to bear the hardness and sharpness of stones, without a covering made\n\
\ from the skin of some other brute; that my whole body wanted a fence\n\
\ against heat and cold, which I was forced to put on and off every day,\n\
\ with tediousness and trouble: and lastly, that he observed every animal\n\
\ in this country naturally to abhor the _Yahoos_, whom the weaker avoided,\n\
\ and the stronger drove from them.  So that, supposing us to have the gift\n\
\ of reason, he could not see how it were possible to cure that natural\n\
\ antipathy, which every creature discovered against us; nor consequently\n\
\ how we could tame and render them serviceable.  However, he would,\" as he\n\
\ said, \"debate the matter no farther, because he was more desirous to know\n\
\ my own story, the country where I was born, and the several actions and\n\
\ events of my life, before I came hither.\"\n\
\ \n\
\ I assured him, \"how extremely desirous I was that he should be satisfied\n\
\ on every point; but I doubted much, whether it would be possible for me\n\
\ to explain myself on several subjects, whereof his honour could have no\n\
\ conception; because I saw nothing in his country to which I could\n\
\ resemble them; that, however, I would do my best, and strive to express\n\
\ myself by similitudes, humbly desiring his assistance when I wanted\n\
\ proper words;\" which he was pleased to promise me.\n\
\ \n\
\ I said, \"my birth was of honest parents, in an island called England;\n\
\ which was remote from his country, as many days' journey as the strongest\n\
\ of his honour's servants could travel in the annual course of the sun;\n\
\ that I was bred a surgeon, whose trade it is to cure wounds and hurts in\n\
\ the body, gotten by accident or violence; that my country was governed by\n\
\ a female man, whom we called queen; that I left it to get riches, whereby\n\
\ I might maintain myself and family, when I should return; that, in my\n\
\ last voyage, I was commander of the ship, and had about fifty _Yahoos_\n\
\ under me, many of which died at sea, and I was forced to supply them by\n\
\ others picked out from several nations; that our ship was twice in danger\n\
\ of being sunk, the first time by a great storm, and the second by\n\
\ striking against a rock.\"  Here my master interposed, by asking me, \"how\n\
\ I could persuade strangers, out of different countries, to venture with\n\
\ me, after the losses I had sustained, and the hazards I had run?\"  I\n\
\ said, \"they were fellows of desperate fortunes, forced to fly from the\n\
\ places of their birth on account of their poverty or their crimes.  Some\n\
\ were undone by lawsuits; others spent all they had in drinking, whoring,\n\
\ and gaming; others fled for treason; many for murder, theft, poisoning,\n\
\ robbery, perjury, forgery, coining false money, for committing rapes, or\n\
\ sodomy; for flying from their colours, or deserting to the enemy; and\n\
\ most of them had broken prison; none of these durst return to their\n\
\ native countries, for fear of being hanged, or of starving in a jail; and\n\
\ therefore they were under the necessity of seeking a livelihood in other\n\
\ places.\"\n\
\ \n\
\ During this discourse, my master was pleased to interrupt me several\n\
\ times.  I had made use of many circumlocutions in describing to him the\n\
\ nature of the several crimes for which most of our crew had been forced\n\
\ to fly their country.  This labour took up several days' conversation,\n\
\ before he was able to comprehend me.  He was wholly at a loss to know\n\
\ what could be the use or necessity of practising those vices.  To clear\n\
\ up which, I endeavoured to give some ideas of the desire of power and\n\
\ riches; of the terrible effects of lust, intemperance, malice, and envy.\n\
\ All this I was forced to define and describe by putting cases and making\n\
\ suppositions.  After which, like one whose imagination was struck with\n\
\ something never seen or heard of before, he would lift up his eyes with\n\
\ amazement and indignation.  Power, government, war, law, punishment, and\n\
\ a thousand other things, had no terms wherein that language could express\n\
\ them, which made the difficulty almost insuperable, to give my master any\n\
\ conception of what I meant.  But being of an excellent understanding,\n\
\ much improved by contemplation and converse, he at last arrived at a\n\
\ competent knowledge of what human nature, in our parts of the world, is\n\
\ capable to perform, and desired I would give him some particular account\n\
\ of that land which we call Europe, but especially of my own country.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER V.\n\
\ \n\
\ \n\
\ The author at his master's command, informs him of the state of England.\n\
\ The causes of war among the princes of Europe.  The author begins to\n\
\ explain the English constitution.\n\
\ \n\
\ The reader may please to observe, that the following extract of many\n\
\ conversations I had with my master, contains a summary of the most\n\
\ material points which were discoursed at several times for above two\n\
\ years; his honour often desiring fuller satisfaction, as I farther\n\
\ improved in the _Houyhnhnm_ tongue.  I laid before him, as well as I\n\
\ could, the whole state of Europe; I discoursed of trade and manufactures,\n\
\ of arts and sciences; and the answers I gave to all the questions he\n\
\ made, as they arose upon several subjects, were a fund of conversation\n\
\ not to be exhausted.  But I shall here only set down the substance of\n\
\ what passed between us concerning my own country, reducing it in order as\n\
\ well as I can, without any regard to time or other circumstances, while I\n\
\ strictly adhere to truth.  My only concern is, that I shall hardly be\n\
\ able to do justice to my master's arguments and expressions, which must\n\
\ needs suffer by my want of capacity, as well as by a translation into our\n\
\ barbarous English.\n\
\ \n\
\ In obedience, therefore, to his honour's commands, I related to him the\n\
\ Revolution under the Prince of Orange; the long war with France, entered\n\
\ into by the said prince, and renewed by his successor, the present queen,\n\
\ wherein the greatest powers of Christendom were engaged, and which still\n\
\ continued: I computed, at his request, \"that about a million of _Yahoos_\n\
\ might have been killed in the whole progress of it; and perhaps a hundred\n\
\ or more cities taken, and five times as many ships burnt or sunk.\"\n\
\ \n\
\ He asked me, \"what were the usual causes or motives that made one country\n\
\ go to war with another?\"  I answered \"they were innumerable; but I should\n\
\ only mention a few of the chief.  Sometimes the ambition of princes, who\n\
\ never think they have land or people enough to govern; sometimes the\n\
\ corruption of ministers, who engage their master in a war, in order to\n\
\ stifle or divert the clamour of the subjects against their evil\n\
\ administration.  Difference in opinions has cost many millions of lives:\n\
\ for instance, whether flesh be bread, or bread be flesh; whether the\n\
\ juice of a certain berry be blood or wine; whether whistling be a vice or\n\
\ a virtue; whether it be better to kiss a post, or throw it into the fire;\n\
\ what is the best colour for a coat, whether black, white, red, or gray;\n\
\ and whether it should be long or short, narrow or wide, dirty or clean;\n\
\ with many more.  Neither are any wars so furious and bloody, or of so\n\
\ long a continuance, as those occasioned by difference in opinion,\n\
\ especially if it be in things indifferent.\n\
\ \n\
\ \"Sometimes the quarrel between two princes is to decide which of them\n\
\ shall dispossess a third of his dominions, where neither of them pretend\n\
\ to any right.  Sometimes one prince quarrels with another for fear the\n\
\ other should quarrel with him.  Sometimes a war is entered upon, because\n\
\ the enemy is too strong; and sometimes, because he is too weak.\n\
\ Sometimes our neighbours want the things which we have, or have the\n\
\ things which we want, and we both fight, till they take ours, or give us\n\
\ theirs.  It is a very justifiable cause of a war, to invade a country\n\
\ after the people have been wasted by famine, destroyed by pestilence, or\n\
\ embroiled by factions among themselves.  It is justifiable to enter into\n\
\ war against our nearest ally, when one of his towns lies convenient for\n\
\ us, or a territory of land, that would render our dominions round and\n\
\ complete.  If a prince sends forces into a nation, where the people are\n\
\ poor and ignorant, he may lawfully put half of them to death, and make\n\
\ slaves of the rest, in order to civilize and reduce them from their\n\
\ barbarous way of living.  It is a very kingly, honourable, and frequent\n\
\ practice, when one prince desires the assistance of another, to secure\n\
\ him against an invasion, that the assistant, when he has driven out the\n\
\ invader, should seize on the dominions himself, and kill, imprison, or\n\
\ banish, the prince he came to relieve.  Alliance by blood, or marriage,\n\
\ is a frequent cause of war between princes; and the nearer the kindred\n\
\ is, the greater their disposition to quarrel; poor nations are hungry,\n\
\ and rich nations are proud; and pride and hunger will ever be at\n\
\ variance.  For these reasons, the trade of a soldier is held the most\n\
\ honourable of all others; because a soldier is a _Yahoo_ hired to kill,\n\
\ in cold blood, as many of his own species, who have never offended him,\n\
\ as possibly he can.\n\
\ \n\
\ \"There is likewise a kind of beggarly princes in Europe, not able to make\n\
\ war by themselves, who hire out their troops to richer nations, for so\n\
\ much a day to each man; of which they keep three-fourths to themselves,\n\
\ and it is the best part of their maintenance: such are those in many\n\
\ northern parts of Europe.\"\n\
\ \n\
\ \"What you have told me,\" said my master, \"upon the subject of war, does\n\
\ indeed discover most admirably the effects of that reason you pretend to:\n\
\ however, it is happy that the shame is greater than the danger; and that\n\
\ nature has left you utterly incapable of doing much mischief.  For, your\n\
\ mouths lying flat with your faces, you can hardly bite each other to any\n\
\ purpose, unless by consent.  Then as to the claws upon your feet before\n\
\ and behind, they are so short and tender, that one of our _Yahoos_ would\n\
\ drive a dozen of yours before him.  And therefore, in recounting the\n\
\ numbers of those who have been killed in battle, I cannot but think you\n\
\ have said the thing which is not.\"\n\
\ \n\
\ I could not forbear shaking my head, and smiling a little at his\n\
\ ignorance.  And being no stranger to the art of war, I gave him a\n\
\ description of cannons, culverins, muskets, carabines, pistols, bullets,\n\
\ powder, swords, bayonets, battles, sieges, retreats, attacks, undermines,\n\
\ countermines, bombardments, sea fights, ships sunk with a thousand men,\n\
\ twenty thousand killed on each side, dying groans, limbs flying in the\n\
\ air, smoke, noise, confusion, trampling to death under horses' feet,\n\
\ flight, pursuit, victory; fields strewed with carcases, left for food to\n\
\ dogs and wolves and birds of prey; plundering, stripping, ravishing,\n\
\ burning, and destroying.  And to set forth the valour of my own dear\n\
\ countrymen, I assured him, \"that I had seen them blow up a hundred\n\
\ enemies at once in a siege, and as many in a ship, and beheld the dead\n\
\ bodies drop down in pieces from the clouds, to the great diversion of the\n\
\ spectators.\"\n\
\ \n\
\ I was going on to more particulars, when my master commanded me silence.\n\
\ He said, \"whoever understood the nature of _Yahoos_, might easily believe\n\
\ it possible for so vile an animal to be capable of every action I had\n\
\ named, if their strength and cunning equalled their malice.  But as my\n\
\ discourse had increased his abhorrence of the whole species, so he found\n\
\ it gave him a disturbance in his mind to which he was wholly a stranger\n\
\ before.  He thought his ears, being used to such abominable words, might,\n\
\ by degrees, admit them with less detestation: that although he hated the\n\
\ _Yahoos_ of this country, yet he no more blamed them for their odious\n\
\ qualities, than he did a _gnnayh_ (a bird of prey) for its cruelty, or a\n\
\ sharp stone for cutting his hoof.  But when a creature pretending to\n\
\ reason could be capable of such enormities, he dreaded lest the\n\
\ corruption of that faculty might be worse than brutality itself.  He\n\
\ seemed therefore confident, that, instead of reason we were only\n\
\ possessed of some quality fitted to increase our natural vices; as the\n\
\ reflection from a troubled stream returns the image of an ill shapen\n\
\ body, not only larger but more distorted.\"\n\
\ \n\
\ He added, \"that he had heard too much upon the subject of war, both in\n\
\ this and some former discourses.  There was another point, which a little\n\
\ perplexed him at present.  I had informed him, that some of our crew left\n\
\ their country on account of being ruined by law; that I had already\n\
\ explained the meaning of the word; but he was at a loss how it should\n\
\ come to pass, that the law, which was intended for every man's\n\
\ preservation, should be any man's ruin.  Therefore he desired to be\n\
\ further satisfied what I meant by law, and the dispensers thereof,\n\
\ according to the present practice in my own country; because he thought\n\
\ nature and reason were sufficient guides for a reasonable animal, as we\n\
\ pretended to be, in showing us what he ought to do, and what to avoid.\"\n\
\ \n\
\ I assured his honour, \"that the law was a science in which I had not much\n\
\ conversed, further than by employing advocates, in vain, upon some\n\
\ injustices that had been done me: however, I would give him all the\n\
\ satisfaction I was able.\"\n\
\ \n\
\ I said, \"there was a society of men among us, bred up from their youth in\n\
\ the art of proving, by words multiplied for the purpose, that white is\n\
\ black, and black is white, according as they are paid.  To this society\n\
\ all the rest of the people are slaves.  For example, if my neighbour has\n\
\ a mind to my cow, he has a lawyer to prove that he ought to have my cow\n\
\ from me.  I must then hire another to defend my right, it being against\n\
\ all rules of law that any man should be allowed to speak for himself.\n\
\ Now, in this case, I, who am the right owner, lie under two great\n\
\ disadvantages: first, my lawyer, being practised almost from his cradle\n\
\ in defending falsehood, is quite out of his element when he would be an\n\
\ advocate for justice, which is an unnatural office he always attempts\n\
\ with great awkwardness, if not with ill-will.  The second disadvantage\n\
\ is, that my lawyer must proceed with great caution, or else he will be\n\
\ reprimanded by the judges, and abhorred by his brethren, as one that\n\
\ would lessen the practice of the law.  And therefore I have but two\n\
\ methods to preserve my cow.  The first is, to gain over my adversary's\n\
\ lawyer with a double fee, who will then betray his client by insinuating\n\
\ that he hath justice on his side.  The second way is for my lawyer to\n\
\ make my cause appear as unjust as he can, by allowing the cow to belong\n\
\ to my adversary: and this, if it be skilfully done, will certainly\n\
\ bespeak the favour of the bench.  Now your honour is to know, that these\n\
\ judges are persons appointed to decide all controversies of property, as\n\
\ well as for the trial of criminals, and picked out from the most\n\
\ dexterous lawyers, who are grown old or lazy; and having been biassed all\n\
\ their lives against truth and equity, lie under such a fatal necessity of\n\
\ favouring fraud, perjury, and oppression, that I have known some of them\n\
\ refuse a large bribe from the side where justice lay, rather than injure\n\
\ the faculty, by doing any thing unbecoming their nature or their office.\n\
\ \n\
\ \"It is a maxim among these lawyers that whatever has been done before,\n\
\ may legally be done again: and therefore they take special care to record\n\
\ all the decisions formerly made against common justice, and the general\n\
\ reason of mankind.  These, under the name of precedents, they produce as\n\
\ authorities to justify the most iniquitous opinions; and the judges never\n\
\ fail of directing accordingly.\n\
\ \n\
\ \"In pleading, they studiously avoid entering into the merits of the\n\
\ cause; but are loud, violent, and tedious, in dwelling upon all\n\
\ circumstances which are not to the purpose.  For instance, in the case\n\
\ already mentioned; they never desire to know what claim or title my\n\
\ adversary has to my cow; but whether the said cow were red or black; her\n\
\ horns long or short; whether the field I graze her in be round or square;\n\
\ whether she was milked at home or abroad; what diseases she is subject\n\
\ to, and the like; after which they consult precedents, adjourn the cause\n\
\ from time to time, and in ten, twenty, or thirty years, come to an issue.\n\
\ \n\
\ \"It is likewise to be observed, that this society has a peculiar cant and\n\
\ jargon of their own, that no other mortal can understand, and wherein all\n\
\ their laws are written, which they take special care to multiply; whereby\n\
\ they have wholly confounded the very essence of truth and falsehood, of\n\
\ right and wrong; so that it will take thirty years to decide, whether the\n\
\ field left me by my ancestors for six generations belongs to me, or to a\n\
\ stranger three hundred miles off.\n\
\ \n\
\ \"In the trial of persons accused for crimes against the state, the method\n\
\ is much more short and commendable: the judge first sends to sound the\n\
\ disposition of those in power, after which he can easily hang or save a\n\
\ criminal, strictly preserving all due forms of law.\"\n\
\ \n\
\ Here my master interposing, said, \"it was a pity, that creatures endowed\n\
\ with such prodigious abilities of mind, as these lawyers, by the\n\
\ description I gave of them, must certainly be, were not rather encouraged\n\
\ to be instructors of others in wisdom and knowledge.\"  In answer to which\n\
\ I assured his honour, \"that in all points out of their own trade, they\n\
\ were usually the most ignorant and stupid generation among us, the most\n\
\ despicable in common conversation, avowed enemies to all knowledge and\n\
\ learning, and equally disposed to pervert the general reason of mankind\n\
\ in every other subject of discourse as in that of their own profession.\"\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER VI.\n\
\ \n\
\ \n\
\ A continuation of the state of England under Queen Anne.  The character\n\
\ of a first minister of state in European courts.\n\
\ \n\
\ My master was yet wholly at a loss to understand what motives could\n\
\ incite this race of lawyers to perplex, disquiet, and weary themselves,\n\
\ and engage in a confederacy of injustice, merely for the sake of injuring\n\
\ their fellow-animals; neither could he comprehend what I meant in saying,\n\
\ they did it for hire.  Whereupon I was at much pains to describe to him\n\
\ the use of money, the materials it was made of, and the value of the\n\
\ metals; \"that when a _Yahoo_ had got a great store of this precious\n\
\ substance, he was able to purchase whatever he had a mind to; the finest\n\
\ clothing, the noblest houses, great tracts of land, the most costly meats\n\
\ and drinks, and have his choice of the most beautiful females.  Therefore\n\
\ since money alone was able to perform all these feats, our _Yahoos_\n\
\ thought they could never have enough of it to spend, or to save, as they\n\
\ found themselves inclined, from their natural bent either to profusion or\n\
\ avarice; that the rich man enjoyed the fruit of the poor man's labour,\n\
\ and the latter were a thousand to one in proportion to the former; that\n\
\ the bulk of our people were forced to live miserably, by labouring every\n\
\ day for small wages, to make a few live plentifully.\"\n\
\ \n\
\ I enlarged myself much on these, and many other particulars to the same\n\
\ purpose; but his honour was still to seek; for he went upon a\n\
\ supposition, that all animals had a title to their share in the\n\
\ productions of the earth, and especially those who presided over the\n\
\ rest.  Therefore he desired I would let him know, \"what these costly\n\
\ meats were, and how any of us happened to want them?\"  Whereupon I\n\
\ enumerated as many sorts as came into my head, with the various methods\n\
\ of dressing them, which could not be done without sending vessels by sea\n\
\ to every part of the world, as well for liquors to drink as for sauces\n\
\ and innumerable other conveniences.  I assured him \"that this whole globe\n\
\ of earth must be at least three times gone round before one of our better\n\
\ female _Yahoos_ could get her breakfast, or a cup to put it in.\"  He said\n\
\ \"that must needs be a miserable country which cannot furnish food for its\n\
\ own inhabitants.  But what he chiefly wondered at was, how such vast\n\
\ tracts of ground as I described should be wholly without fresh water, and\n\
\ the people put to the necessity of sending over the sea for drink.\"  I\n\
\ replied \"that England (the dear place of my nativity) was computed to\n\
\ produce three times the quantity of food more than its inhabitants are\n\
\ able to consume, as well as liquors extracted from grain, or pressed out\n\
\ of the fruit of certain trees, which made excellent drink, and the same\n\
\ proportion in every other convenience of life.  But, in order to feed the\n\
\ luxury and intemperance of the males, and the vanity of the females, we\n\
\ sent away the greatest part of our necessary things to other countries,\n\
\ whence, in return, we brought the materials of diseases, folly, and vice,\n\
\ to spend among ourselves.  Hence it follows of necessity, that vast\n\
\ numbers of our people are compelled to seek their livelihood by begging,\n\
\ robbing, stealing, cheating, pimping, flattering, suborning, forswearing,\n\
\ forging, gaming, lying, fawning, hectoring, voting, scribbling,\n\
\ star-gazing, poisoning, whoring, canting, libelling, freethinking, and\n\
\ the like occupations:\" every one of which terms I was at much pains to\n\
\ make him understand.\n\
\ \n\
\ \"That wine was not imported among us from foreign countries to supply the\n\
\ want of water or other drinks, but because it was a sort of liquid which\n\
\ made us merry by putting us out of our senses, diverted all melancholy\n\
\ thoughts, begat wild extravagant imaginations in the brain, raised our\n\
\ hopes and banished our fears, suspended every office of reason for a\n\
\ time, and deprived us of the use of our limbs, till we fell into a\n\
\ profound sleep; although it must be confessed, that we always awaked sick\n\
\ and dispirited; and that the use of this liquor filled us with diseases\n\
\ which made our lives uncomfortable and short.\n\
\ \n\
\ \"But beside all this, the bulk of our people supported themselves by\n\
\ furnishing the necessities or conveniences of life to the rich and to\n\
\ each other.  For instance, when I am at home, and dressed as I ought to\n\
\ be, I carry on my body the workmanship of a hundred tradesmen; the\n\
\ building and furniture of my house employ as many more, and five times\n\
\ the number to adorn my wife.\"\n\
\ \n\
\ I was going on to tell him of another sort of people, who get their\n\
\ livelihood by attending the sick, having, upon some occasions, informed\n\
\ his honour that many of my crew had died of diseases.  But here it was\n\
\ with the utmost difficulty that I brought him to apprehend what I meant.\n\
\ \"He could easily conceive, that a _Houyhnhnm_, grew weak and heavy a few\n\
\ days before his death, or by some accident might hurt a limb; but that\n\
\ nature, who works all things to perfection, should suffer any pains to\n\
\ breed in our bodies, he thought impossible, and desired to know the\n\
\ reason of so unaccountable an evil.\"\n\
\ \n\
\ I told him \"we fed on a thousand things which operated contrary to each\n\
\ other; that we ate when we were not hungry, and drank without the\n\
\ provocation of thirst; that we sat whole nights drinking strong liquors,\n\
\ without eating a bit, which disposed us to sloth, inflamed our bodies,\n\
\ and precipitated or prevented digestion; that prostitute female _Yahoos_\n\
\ acquired a certain malady, which bred rottenness in the bones of those\n\
\ who fell into their embraces; that this, and many other diseases, were\n\
\ propagated from father to son; so that great numbers came into the world\n\
\ with complicated maladies upon them; that it would be endless to give him\n\
\ a catalogue of all diseases incident to human bodies, for they would not\n\
\ be fewer than five or six hundred, spread over every limb and joint--in\n\
\ short, every part, external and intestine, having diseases appropriated\n\
\ to itself.  To remedy which, there was a sort of people bred up among us\n\
\ in the profession, or pretence, of curing the sick.  And because I had\n\
\ some skill in the faculty, I would, in gratitude to his honour, let him\n\
\ know the whole mystery and method by which they proceed.\n\
\ \n\
\ \"Their fundamental is, that all diseases arise from repletion; whence\n\
\ they conclude, that a great evacuation of the body is necessary, either\n\
\ through the natural passage or upwards at the mouth.  Their next business\n\
\ is from herbs, minerals, gums, oils, shells, salts, juices, sea-weed,\n\
\ excrements, barks of trees, serpents, toads, frogs, spiders, dead men's\n\
\ flesh and bones, birds, beasts, and fishes, to form a composition, for\n\
\ smell and taste, the most abominable, nauseous, and detestable, they can\n\
\ possibly contrive, which the stomach immediately rejects with loathing,\n\
\ and this they call a vomit; or else, from the same store-house, with some\n\
\ other poisonous additions, they command us to take in at the orifice\n\
\ above or below (just as the physician then happens to be disposed) a\n\
\ medicine equally annoying and disgustful to the bowels; which, relaxing\n\
\ the belly, drives down all before it; and this they call a purge, or a\n\
\ clyster.  For nature (as the physicians allege) having intended the\n\
\ superior anterior orifice only for the intromission of solids and\n\
\ liquids, and the inferior posterior for ejection, these artists\n\
\ ingeniously considering that in all diseases nature is forced out of her\n\
\ seat, therefore, to replace her in it, the body must be treated in a\n\
\ manner directly contrary, by interchanging the use of each orifice;\n\
\ forcing solids and liquids in at the anus, and making evacuations at the\n\
\ mouth.\n\
\ \n\
\ \"But, besides real diseases, we are subject to many that are only\n\
\ imaginary, for which the physicians have invented imaginary cures; these\n\
\ have their several names, and so have the drugs that are proper for them;\n\
\ and with these our female _Yahoos_ are always infested.\n\
\ \n\
\ \"One great excellency in this tribe, is their skill at prognostics,\n\
\ wherein they seldom fail; their predictions in real diseases, when they\n\
\ rise to any degree of malignity, generally portending death, which is\n\
\ always in their power, when recovery is not: and therefore, upon any\n\
\ unexpected signs of amendment, after they have pronounced their sentence,\n\
\ rather than be accused as false prophets, they know how to approve their\n\
\ sagacity to the world, by a seasonable dose.\n\
\ \n\
\ \"They are likewise of special use to husbands and wives who are grown\n\
\ weary of their mates; to eldest sons, to great ministers of state, and\n\
\ often to princes.\"\n\
\ \n\
\ I had formerly, upon occasion, discoursed with my master upon the nature\n\
\ of government in general, and particularly of our own excellent\n\
\ constitution, deservedly the wonder and envy of the whole world.  But\n\
\ having here accidentally mentioned a minister of state, he commanded me,\n\
\ some time after, to inform him, \"what species of _Yahoo_ I particularly\n\
\ meant by that appellation.\"\n\
\ \n\
\ I told him, \"that a first or chief minister of state, who was the person\n\
\ I intended to describe, was the creature wholly exempt from joy and\n\
\ grief, love and hatred, pity and anger; at least, makes use of no other\n\
\ passions, but a violent desire of wealth, power, and titles; that he\n\
\ applies his words to all uses, except to the indication of his mind; that\n\
\ he never tells a truth but with an intent that you should take it for a\n\
\ lie; nor a lie, but with a design that you should take it for a truth;\n\
\ that those he speaks worst of behind their backs are in the surest way of\n\
\ preferment; and whenever he begins to praise you to others, or to\n\
\ yourself, you are from that day forlorn.  The worst mark you can receive\n\
\ is a promise, especially when it is confirmed with an oath; after which,\n\
\ every wise man retires, and gives over all hopes.\n\
\ \n\
\ \"There are three methods, by which a man may rise to be chief minister.\n\
\ The first is, by knowing how, with prudence, to dispose of a wife, a\n\
\ daughter, or a sister; the second, by betraying or undermining his\n\
\ predecessor; and the third is, by a furious zeal, in public assemblies,\n\
\ against the corruption's of the court.  But a wise prince would rather\n\
\ choose to employ those who practise the last of these methods; because\n\
\ such zealots prove always the most obsequious and subservient to the will\n\
\ and passions of their master.  That these ministers, having all\n\
\ employments at their disposal, preserve themselves in power, by bribing\n\
\ the majority of a senate or great council; and at last, by an expedient,\n\
\ called an act of indemnity\" (whereof I described the nature to him),\n\
\ \"they secure themselves from after-reckonings, and retire from the public\n\
\ laden with the spoils of the nation.\n\
\ \n\
\ \"The palace of a chief minister is a seminary to breed up others in his\n\
\ own trade: the pages, lackeys, and porters, by imitating their master,\n\
\ become ministers of state in their several districts, and learn to excel\n\
\ in the three principal ingredients, of insolence, lying, and bribery.\n\
\ Accordingly, they have a subaltern court paid to them by persons of the\n\
\ best rank; and sometimes by the force of dexterity and impudence, arrive,\n\
\ through several gradations, to be successors to their lord.\n\
\ \n\
\ \"He is usually governed by a decayed wench, or favourite footman, who are\n\
\ the tunnels through which all graces are conveyed, and may properly be\n\
\ called, in the last resort, the governors of the kingdom.\"\n\
\ \n\
\ One day, in discourse, my master, having heard me mention the nobility of\n\
\ my country, was pleased to make me a compliment which I could not pretend\n\
\ to deserve: \"that he was sure I must have been born of some noble family,\n\
\ because I far exceeded in shape, colour, and cleanliness, all the\n\
\ _Yahoos_ of his nation, although I seemed to fail in strength and\n\
\ agility, which must be imputed to my different way of living from those\n\
\ other brutes; and besides I was not only endowed with the faculty of\n\
\ speech, but likewise with some rudiments of reason, to a degree that,\n\
\ with all his acquaintance, I passed for a prodigy.\"\n\
\ \n\
\ He made me observe, \"that among the _Houyhnhnms_, the white, the sorrel,\n\
\ and the iron-gray, were not so exactly shaped as the bay, the\n\
\ dapple-gray, and the black; nor born with equal talents of mind, or a\n\
\ capacity to improve them; and therefore continued always in the condition\n\
\ of servants, without ever aspiring to match out of their own race, which\n\
\ in that country would be reckoned monstrous and unnatural.\"\n\
\ \n\
\ I made his honour my most humble acknowledgments for the good opinion he\n\
\ was pleased to conceive of me, but assured him at the same time, \"that my\n\
\ birth was of the lower sort, having been born of plain honest parents,\n\
\ who were just able to give me a tolerable education; that nobility, among\n\
\ us, was altogether a different thing from the idea he had of it; that our\n\
\ young noblemen are bred from their childhood in idleness and luxury;\n\
\ that, as soon as years will permit, they consume their vigour, and\n\
\ contract odious diseases among lewd females; and when their fortunes are\n\
\ almost ruined, they marry some woman of mean birth, disagreeable person,\n\
\ and unsound constitution (merely for the sake of money), whom they hate\n\
\ and despise.  That the productions of such marriages are generally\n\
\ scrofulous, rickety, or deformed children; by which means the family\n\
\ seldom continues above three generations, unless the wife takes care to\n\
\ provide a healthy father, among her neighbours or domestics, in order to\n\
\ improve and continue the breed.  That a weak diseased body, a meagre\n\
\ countenance, and sallow complexion, are the true marks of noble blood;\n\
\ and a healthy robust appearance is so disgraceful in a man of quality,\n\
\ that the world concludes his real father to have been a groom or a\n\
\ coachman.  The imperfections of his mind run parallel with those of his\n\
\ body, being a composition of spleen, dullness, ignorance, caprice,\n\
\ sensuality, and pride.\n\
\ \n\
\ \"Without the consent of this illustrious body, no law can be enacted,\n\
\ repealed, or altered: and these nobles have likewise the decision of all\n\
\ our possessions, without appeal.\" {514}\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER VII.\n\
\ \n\
\ \n\
\ The author's great love of his native country.  His master's observations\n\
\ upon the constitution and administration of England, as described by the\n\
\ author, with parallel cases and comparisons.  His master's observations\n\
\ upon human nature.\n\
\ \n\
\ The reader may be disposed to wonder how I could prevail on myself to\n\
\ give so free a representation of my own species, among a race of mortals\n\
\ who are already too apt to conceive the vilest opinion of humankind, from\n\
\ that entire congruity between me and their _Yahoos_.  But I must freely\n\
\ confess, that the many virtues of those excellent quadrupeds, placed in\n\
\ opposite view to human corruptions, had so far opened my eyes and\n\
\ enlarged my understanding, that I began to view the actions and passions\n\
\ of man in a very different light, and to think the honour of my own kind\n\
\ not worth managing; which, besides, it was impossible for me to do,\n\
\ before a person of so acute a judgment as my master, who daily convinced\n\
\ me of a thousand faults in myself, whereof I had not the least perception\n\
\ before, and which, with us, would never be numbered even among human\n\
\ infirmities.  I had likewise learned, from his example, an utter\n\
\ detestation of all falsehood or disguise; and truth appeared so amiable\n\
\ to me, that I determined upon sacrificing every thing to it.\n\
\ \n\
\ Let me deal so candidly with the reader as to confess that there was yet\n\
\ a much stronger motive for the freedom I took in my representation of\n\
\ things.  I had not yet been a year in this country before I contracted\n\
\ such a love and veneration for the inhabitants, that I entered on a firm\n\
\ resolution never to return to humankind, but to pass the rest of my life\n\
\ among these admirable _Houyhnhnms_, in the contemplation and practice of\n\
\ every virtue, where I could have no example or incitement to vice.  But\n\
\ it was decreed by fortune, my perpetual enemy, that so great a felicity\n\
\ should not fall to my share.  However, it is now some comfort to reflect,\n\
\ that in what I said of my countrymen, I extenuated their faults as much\n\
\ as I durst before so strict an examiner; and upon every article gave as\n\
\ favourable a turn as the matter would bear.  For, indeed, who is there\n\
\ alive that will not be swayed by his bias and partiality to the place of\n\
\ his birth?\n\
\ \n\
\ I have related the substance of several conversations I had with my\n\
\ master during the greatest part of the time I had the honour to be in his\n\
\ service; but have, indeed, for brevity sake, omitted much more than is\n\
\ here set down.\n\
\ \n\
\ When I had answered all his questions, and his curiosity seemed to be\n\
\ fully satisfied, he sent for me one morning early, and commanded me to\n\
\ sit down at some distance (an honour which he had never before conferred\n\
\ upon me).  He said, \"he had been very seriously considering my whole\n\
\ story, as far as it related both to myself and my country; that he looked\n\
\ upon us as a sort of animals, to whose share, by what accident he could\n\
\ not conjecture, some small pittance of reason had fallen, whereof we made\n\
\ no other use, than by its assistance, to aggravate our natural\n\
\ corruptions, and to acquire new ones, which nature had not given us; that\n\
\ we disarmed ourselves of the few abilities she had bestowed; had been\n\
\ very successful in multiplying our original wants, and seemed to spend\n\
\ our whole lives in vain endeavours to supply them by our own inventions;\n\
\ that, as to myself, it was manifest I had neither the strength nor\n\
\ agility of a common _Yahoo_; that I walked infirmly on my hinder feet;\n\
\ had found out a contrivance to make my claws of no use or defence, and to\n\
\ remove the hair from my chin, which was intended as a shelter from the\n\
\ sun and the weather: lastly, that I could neither run with speed, nor\n\
\ climb trees like my brethren,\" as he called them, \"the _Yahoos_ in his\n\
\ country.\n\
\ \n\
\ \"That our institutions of government and law were plainly owing to our\n\
\ gross defects in reason, and by consequence in virtue; because reason\n\
\ alone is sufficient to govern a rational creature; which was, therefore,\n\
\ a character we had no pretence to challenge, even from the account I had\n\
\ given of my own people; although he manifestly perceived, that, in order\n\
\ to favour them, I had concealed many particulars, and often said the\n\
\ thing which was not.\n\
\ \n\
\ \"He was the more confirmed in this opinion, because, he observed, that as\n\
\ I agreed in every feature of my body with other _Yahoos_, except where it\n\
\ was to my real disadvantage in point of strength, speed, and activity,\n\
\ the shortness of my claws, and some other particulars where nature had no\n\
\ part; so from the representation I had given him of our lives, our\n\
\ manners, and our actions, he found as near a resemblance in the\n\
\ disposition of our minds.\"  He said, \"the _Yahoos_ were known to hate one\n\
\ another, more than they did any different species of animals; and the\n\
\ reason usually assigned was, the odiousness of their own shapes, which\n\
\ all could see in the rest, but not in themselves.  He had therefore begun\n\
\ to think it not unwise in us to cover our bodies, and by that invention\n\
\ conceal many of our deformities from each other, which would else be\n\
\ hardly supportable.  But he now found he had been mistaken, and that the\n\
\ dissensions of those brutes in his country were owing to the same cause\n\
\ with ours, as I had described them.  For if,\" said he, \"you throw among\n\
\ five _Yahoos_ as much food as would be sufficient for fifty, they will,\n\
\ instead of eating peaceably, fall together by the ears, each single one\n\
\ impatient to have all to itself; and therefore a servant was usually\n\
\ employed to stand by while they were feeding abroad, and those kept at\n\
\ home were tied at a distance from each other: that if a cow died of age\n\
\ or accident, before a _Houyhnhnm_ could secure it for his own _Yahoos_,\n\
\ those in the neighbourhood would come in herds to seize it, and then\n\
\ would ensue such a battle as I had described, with terrible wounds made\n\
\ by their claws on both sides, although they seldom were able to kill one\n\
\ another, for want of such convenient instruments of death as we had\n\
\ invented.  At other times, the like battles have been fought between the\n\
\ _Yahoos_ of several neighbourhoods, without any visible cause; those of\n\
\ one district watching all opportunities to surprise the next, before they\n\
\ are prepared.  But if they find their project has miscarried, they return\n\
\ home, and, for want of enemies, engage in what I call a civil war among\n\
\ themselves.\n\
\ \n\
\ \"That in some fields of his country there are certain shining stones of\n\
\ several colours, whereof the _Yahoos_ are violently fond: and when part\n\
\ of these stones is fixed in the earth, as it sometimes happens, they will\n\
\ dig with their claws for whole days to get them out; then carry them\n\
\ away, and hide them by heaps in their kennels; but still looking round\n\
\ with great caution, for fear their comrades should find out their\n\
\ treasure.\"  My master said, \"he could never discover the reason of this\n\
\ unnatural appetite, or how these stones could be of any use to a _Yahoo_;\n\
\ but now he believed it might proceed from the same principle of avarice\n\
\ which I had ascribed to mankind.  That he had once, by way of experiment,\n\
\ privately removed a heap of these stones from the place where one of his\n\
\ _Yahoos_ had buried it; whereupon the sordid animal, missing his\n\
\ treasure, by his loud lamenting brought the whole herd to the place,\n\
\ there miserably howled, then fell to biting and tearing the rest, began\n\
\ to pine away, would neither eat, nor sleep, nor work, till he ordered a\n\
\ servant privately to convey the stones into the same hole, and hide them\n\
\ as before; which, when his _Yahoo_ had found, he presently recovered his\n\
\ spirits and good humour, but took good care to remove them to a better\n\
\ hiding place, and has ever since been a very serviceable brute.\"\n\
\ \n\
\ My master further assured me, which I also observed myself, \"that in the\n\
\ fields where the shining stones abound, the fiercest and most frequent\n\
\ battles are fought, occasioned by perpetual inroads of the neighbouring\n\
\ _Yahoos_.\"\n\
\ \n\
\ He said, \"it was common, when two _Yahoos_ discovered such a stone in a\n\
\ field, and were contending which of them should be the proprietor, a\n\
\ third would take the advantage, and carry it away from them both;\" which\n\
\ my master would needs contend to have some kind of resemblance with our\n\
\ suits at law; wherein I thought it for our credit not to undeceive him;\n\
\ since the decision he mentioned was much more equitable than many decrees\n\
\ among us; because the plaintiff and defendant there lost nothing beside\n\
\ the stone they contended for: whereas our courts of equity would never\n\
\ have dismissed the cause, while either of them had any thing left.\n\
\ \n\
\ My master, continuing his discourse, said, \"there was nothing that\n\
\ rendered the _Yahoos_ more odious, than their undistinguishing appetite\n\
\ to devour every thing that came in their way, whether herbs, roots,\n\
\ berries, the corrupted flesh of animals, or all mingled together: and it\n\
\ was peculiar in their temper, that they were fonder of what they could\n\
\ get by rapine or stealth, at a greater distance, than much better food\n\
\ provided for them at home.  If their prey held out, they would eat till\n\
\ they were ready to burst; after which, nature had pointed out to them a\n\
\ certain root that gave them a general evacuation.\n\
\ \n\
\ \"There was also another kind of root, very juicy, but somewhat rare and\n\
\ difficult to be found, which the _Yahoos_ sought for with much eagerness,\n\
\ and would suck it with great delight; it produced in them the same\n\
\ effects that wine has upon us.  It would make them sometimes hug, and\n\
\ sometimes tear one another; they would howl, and grin, and chatter, and\n\
\ reel, and tumble, and then fall asleep in the mud.\"\n\
\ \n\
\ I did indeed observe that the _Yahoos_ were the only animals in this\n\
\ country subject to any diseases; which, however, were much fewer than\n\
\ horses have among us, and contracted, not by any ill-treatment they meet\n\
\ with, but by the nastiness and greediness of that sordid brute.  Neither\n\
\ has their language any more than a general appellation for those\n\
\ maladies, which is borrowed from the name of the beast, and called\n\
\ _hnea-yahoo_, or _Yahoo's evil_; and the cure prescribed is a mixture of\n\
\ their own dung and urine, forcibly put down the _Yahoo's_ throat.  This I\n\
\ have since often known to have been taken with success, and do here\n\
\ freely recommend it to my countrymen for the public good, as an admirable\n\
\ specific against all diseases produced by repletion.\n\
\ \n\
\ \"As to learning, government, arts, manufactures, and the like,\" my master\n\
\ confessed, \"he could find little or no resemblance between the _Yahoos_\n\
\ of that country and those in ours; for he only meant to observe what\n\
\ parity there was in our natures.  He had heard, indeed, some curious\n\
\ _Houyhnhnms_ observe, that in most herds there was a sort of ruling\n\
\ _Yahoo_ (as among us there is generally some leading or principal stag in\n\
\ a park), who was always more deformed in body, and mischievous in\n\
\ disposition, than any of the rest; that this leader had usually a\n\
\ favourite as like himself as he could get, whose employment was to lick\n\
\ his master's feet and posteriors, and drive the female _Yahoos_ to his\n\
\ kennel; for which he was now and then rewarded with a piece of ass's\n\
\ flesh.  This favourite is hated by the whole herd, and therefore, to\n\
\ protect himself, keeps always near the person of his leader.  He usually\n\
\ continues in office till a worse can be found; but the very moment he is\n\
\ discarded, his successor, at the head of all the _Yahoos_ in that\n\
\ district, young and old, male and female, come in a body, and discharge\n\
\ their excrements upon him from head to foot.  But how far this might be\n\
\ applicable to our courts, and favourites, and ministers of state, my\n\
\ master said I could best determine.\"\n\
\ \n\
\ I durst make no return to this malicious insinuation, which debased human\n\
\ understanding below the sagacity of a common hound, who has judgment\n\
\ enough to distinguish and follow the cry of the ablest dog in the pack,\n\
\ without being ever mistaken.\n\
\ \n\
\ My master told me, \"there were some qualities remarkable in the _Yahoos_,\n\
\ which he had not observed me to mention, or at least very slightly, in\n\
\ the accounts I had given of humankind.\"  He said, \"those animals, like\n\
\ other brutes, had their females in common; but in this they differed,\n\
\ that the she _Yahoo_ would admit the males while she was pregnant; and\n\
\ that the hes would quarrel and fight with the females, as fiercely as\n\
\ with each other; both which practices were such degrees of infamous\n\
\ brutality, as no other sensitive creature ever arrived at.\n\
\ \n\
\ \"Another thing he wondered at in the _Yahoos_, was their strange\n\
\ disposition to nastiness and dirt; whereas there appears to be a natural\n\
\ love of cleanliness in all other animals.\"  As to the two former\n\
\ accusations, I was glad to let them pass without any reply, because I had\n\
\ not a word to offer upon them in defence of my species, which otherwise I\n\
\ certainly had done from my own inclinations.  But I could have easily\n\
\ vindicated humankind from the imputation of singularity upon the last\n\
\ article, if there had been any swine in that country (as unluckily for me\n\
\ there were not), which, although it may be a sweeter quadruped than a\n\
\ _Yahoo_, cannot, I humbly conceive, in justice, pretend to more\n\
\ cleanliness; and so his honour himself must have owned, if he had seen\n\
\ their filthy way of feeding, and their custom of wallowing and sleeping\n\
\ in the mud.\n\
\ \n\
\ My master likewise mentioned another quality which his servants had\n\
\ discovered in several Yahoos, and to him was wholly unaccountable.  He\n\
\ said, \"a fancy would sometimes take a _Yahoo_ to retire into a corner, to\n\
\ lie down, and howl, and groan, and spurn away all that came near him,\n\
\ although he were young and fat, wanted neither food nor water, nor did\n\
\ the servant imagine what could possibly ail him.  And the only remedy\n\
\ they found was, to set him to hard work, after which he would infallibly\n\
\ come to himself.\"  To this I was silent out of partiality to my own kind;\n\
\ yet here I could plainly discover the true seeds of spleen, which only\n\
\ seizes on the lazy, the luxurious, and the rich; who, if they were forced\n\
\ to undergo the same regimen, I would undertake for the cure.\n\
\ \n\
\ His honour had further observed, \"that a female _Yahoo_ would often stand\n\
\ behind a bank or a bush, to gaze on the young males passing by, and then\n\
\ appear, and hide, using many antic gestures and grimaces, at which time\n\
\ it was observed that she had a most offensive smell; and when any of the\n\
\ males advanced, would slowly retire, looking often back, and with a\n\
\ counterfeit show of fear, run off into some convenient place, where she\n\
\ knew the male would follow her.\n\
\ \n\
\ \"At other times, if a female stranger came among them, three or four of\n\
\ her own sex would get about her, and stare, and chatter, and grin, and\n\
\ smell her all over; and then turn off with gestures, that seemed to\n\
\ express contempt and disdain.\"\n\
\ \n\
\ Perhaps my master might refine a little in these speculations, which he\n\
\ had drawn from what he observed himself, or had been told him by others;\n\
\ however, I could not reflect without some amazement, and much sorrow,\n\
\ that the rudiments of lewdness, coquetry, censure, and scandal, should\n\
\ have place by instinct in womankind.\n\
\ \n\
\ I expected every moment that my master would accuse the _Yahoos_ of those\n\
\ unnatural appetites in both sexes, so common among us.  But nature, it\n\
\ seems, has not been so expert a school-mistress; and these politer\n\
\ pleasures are entirely the productions of art and reason on our side of\n\
\ the globe.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER VIII.\n\
\ \n\
\ \n\
\ The author relates several particulars of the _Yahoos_.  The great\n\
\ virtues of the _Houyhnhnms_.  The education and exercise of their youth.\n\
\ Their general assembly.\n\
\ \n\
\ As I ought to have understood human nature much better than I supposed it\n\
\ possible for my master to do, so it was easy to apply the character he\n\
\ gave of the _Yahoos_ to myself and my countrymen; and I believed I could\n\
\ yet make further discoveries, from my own observation.  I therefore often\n\
\ begged his honour to let me go among the herds of _Yahoos_ in the\n\
\ neighbourhood; to which he always very graciously consented, being\n\
\ perfectly convinced that the hatred I bore these brutes would never\n\
\ suffer me to be corrupted by them; and his honour ordered one of his\n\
\ servants, a strong sorrel nag, very honest and good-natured, to be my\n\
\ guard; without whose protection I durst not undertake such adventures.\n\
\ For I have already told the reader how much I was pestered by these\n\
\ odious animals, upon my first arrival; and I afterwards failed very\n\
\ narrowly, three or four times, of falling into their clutches, when I\n\
\ happened to stray at any distance without my hanger.  And I have reason\n\
\ to believe they had some imagination that I was of their own species,\n\
\ which I often assisted myself by stripping up my sleeves, and showing my\n\
\ naked arms and breasts in their sight, when my protector was with me.  At\n\
\ which times they would approach as near as they durst, and imitate my\n\
\ actions after the manner of monkeys, but ever with great signs of hatred;\n\
\ as a tame jackdaw with cap and stockings is always persecuted by the wild\n\
\ ones, when he happens to be got among them.\n\
\ \n\
\ They are prodigiously nimble from their infancy.  However, I once caught\n\
\ a young male of three years old, and endeavoured, by all marks of\n\
\ tenderness, to make it quiet; but the little imp fell a squalling, and\n\
\ scratching, and biting with such violence, that I was forced to let it\n\
\ go; and it was high time, for a whole troop of old ones came about us at\n\
\ the noise, but finding the cub was safe (for away it ran), and my sorrel\n\
\ nag being by, they durst not venture near us.  I observed the young\n\
\ animal's flesh to smell very rank, and the stink was somewhat between a\n\
\ weasel and a fox, but much more disagreeable.  I forgot another\n\
\ circumstance (and perhaps I might have the reader's pardon if it were\n\
\ wholly omitted), that while I held the odious vermin in my hands, it\n\
\ voided its filthy excrements of a yellow liquid substance all over my\n\
\ clothes; but by good fortune there was a small brook hard by, where I\n\
\ washed myself as clean as I could; although I durst not come into my\n\
\ master's presence until I were sufficiently aired.\n\
\ \n\
\ By what I could discover, the _Yahoos_ appear to be the most unteachable\n\
\ of all animals: their capacity never reaching higher than to draw or\n\
\ carry burdens.  Yet I am of opinion, this defect arises chiefly from a\n\
\ perverse, restive disposition; for they are cunning, malicious,\n\
\ treacherous, and revengeful.  They are strong and hardy, but of a\n\
\ cowardly spirit, and, by consequence, insolent, abject, and cruel.  It is\n\
\ observed, that the red haired of both sexes are more libidinous and\n\
\ mischievous than the rest, whom yet they much exceed in strength and\n\
\ activity.\n\
\ \n\
\ The _Houyhnhnms_ keep the _Yahoos_ for present use in huts not far from\n\
\ the house; but the rest are sent abroad to certain fields, where they dig\n\
\ up roots, eat several kinds of herbs, and search about for carrion, or\n\
\ sometimes catch weasels and _luhimuhs_ (a sort of wild rat), which they\n\
\ greedily devour.  Nature has taught them to dig deep holes with their\n\
\ nails on the side of a rising ground, wherein they lie by themselves;\n\
\ only the kennels of the females are larger, sufficient to hold two or\n\
\ three cubs.\n\
\ \n\
\ They swim from their infancy like frogs, and are able to continue long\n\
\ under water, where they often take fish, which the females carry home to\n\
\ their young.  And, upon this occasion, I hope the reader will pardon my\n\
\ relating an odd adventure.\n\
\ \n\
\ Being one day abroad with my protector the sorrel nag, and the weather\n\
\ exceeding hot, I entreated him to let me bathe in a river that was near.\n\
\ He consented, and I immediately stripped myself stark naked, and went\n\
\ down softly into the stream.  It happened that a young female _Yahoo_,\n\
\ standing behind a bank, saw the whole proceeding, and inflamed by desire,\n\
\ as the nag and I conjectured, came running with all speed, and leaped\n\
\ into the water, within five yards of the place where I bathed.  I was\n\
\ never in my life so terribly frightened.  The nag was grazing at some\n\
\ distance, not suspecting any harm.  She embraced me after a most fulsome\n\
\ manner.  I roared as loud as I could, and the nag came galloping towards\n\
\ me, whereupon she quitted her grasp, with the utmost reluctancy, and\n\
\ leaped upon the opposite bank, where she stood gazing and howling all the\n\
\ time I was putting on my clothes.\n\
\ \n\
\ This was a matter of diversion to my master and his family, as well as of\n\
\ mortification to myself.  For now I could no longer deny that I was a\n\
\ real _Yahoo_ in every limb and feature, since the females had a natural\n\
\ propensity to me, as one of their own species.  Neither was the hair of\n\
\ this brute of a red colour (which might have been some excuse for an\n\
\ appetite a little irregular), but black as a sloe, and her countenance\n\
\ did not make an appearance altogether so hideous as the rest of her kind;\n\
\ for I think she could not be above eleven years old.\n\
\ \n\
\ Having lived three years in this country, the reader, I suppose, will\n\
\ expect that I should, like other travellers, give him some account of the\n\
\ manners and customs of its inhabitants, which it was indeed my principal\n\
\ study to learn.\n\
\ \n\
\ As these noble _Houyhnhnms_ are endowed by nature with a general\n\
\ disposition to all virtues, and have no conceptions or ideas of what is\n\
\ evil in a rational creature, so their grand maxim is, to cultivate\n\
\ reason, and to be wholly governed by it.  Neither is reason among them a\n\
\ point problematical, as with us, where men can argue with plausibility on\n\
\ both sides of the question, but strikes you with immediate conviction; as\n\
\ it must needs do, where it is not mingled, obscured, or discoloured, by\n\
\ passion and interest.  I remember it was with extreme difficulty that I\n\
\ could bring my master to understand the meaning of the word opinion, or\n\
\ how a point could be disputable; because reason taught us to affirm or\n\
\ deny only where we are certain; and beyond our knowledge we cannot do\n\
\ either.  So that controversies, wranglings, disputes, and positiveness,\n\
\ in false or dubious propositions, are evils unknown among the\n\
\ _Houyhnhnms_.  In the like manner, when I used to explain to him our\n\
\ several systems of natural philosophy, he would laugh, \"that a creature\n\
\ pretending to reason, should value itself upon the knowledge of other\n\
\ people's conjectures, and in things where that knowledge, if it were\n\
\ certain, could be of no use.\"  Wherein he agreed entirely with the\n\
\ sentiments of Socrates, as Plato delivers them; which I mention as the\n\
\ highest honour I can do that prince of philosophers.  I have often since\n\
\ reflected, what destruction such doctrine would make in the libraries of\n\
\ Europe; and how many paths of fame would be then shut up in the learned\n\
\ world.\n\
\ \n\
\ Friendship and benevolence are the two principal virtues among the\n\
\ _Houyhnhnms_; and these not confined to particular objects, but universal\n\
\ to the whole race; for a stranger from the remotest part is equally\n\
\ treated with the nearest neighbour, and wherever he goes, looks upon\n\
\ himself as at home.  They preserve decency and civility in the highest\n\
\ degrees, but are altogether ignorant of ceremony.  They have no fondness\n\
\ for their colts or foals, but the care they take in educating them\n\
\ proceeds entirely from the dictates of reason.  And I observed my master\n\
\ to show the same affection to his neighbour's issue, that he had for his\n\
\ own.  They will have it that nature teaches them to love the whole\n\
\ species, and it is reason only that makes a distinction of persons, where\n\
\ there is a superior degree of virtue.\n\
\ \n\
\ When the matron _Houyhnhnms_ have produced one of each sex, they no\n\
\ longer accompany with their consorts, except they lose one of their issue\n\
\ by some casualty, which very seldom happens; but in such a case they meet\n\
\ again; or when the like accident befalls a person whose wife is past\n\
\ bearing, some other couple bestow on him one of their own colts, and then\n\
\ go together again until the mother is pregnant.  This caution is\n\
\ necessary, to prevent the country from being overburdened with numbers.\n\
\ But the race of inferior _Houyhnhnms_, bred up to be servants, is not so\n\
\ strictly limited upon this article: these are allowed to produce three of\n\
\ each sex, to be domestics in the noble families.\n\
\ \n\
\ In their marriages, they are exactly careful to choose such colours as\n\
\ will not make any disagreeable mixture in the breed.  Strength is chiefly\n\
\ valued in the male, and comeliness in the female; not upon the account of\n\
\ love, but to preserve the race from degenerating; for where a female\n\
\ happens to excel in strength, a consort is chosen, with regard to\n\
\ comeliness.\n\
\ \n\
\ Courtship, love, presents, jointures, settlements have no place in their\n\
\ thoughts, or terms whereby to express them in their language.  The young\n\
\ couple meet, and are joined, merely because it is the determination of\n\
\ their parents and friends; it is what they see done every day, and they\n\
\ look upon it as one of the necessary actions of a reasonable being.  But\n\
\ the violation of marriage, or any other unchastity, was never heard of;\n\
\ and the married pair pass their lives with the same friendship and mutual\n\
\ benevolence, that they bear to all others of the same species who come in\n\
\ their way, without jealousy, fondness, quarrelling, or discontent.\n\
\ \n\
\ In educating the youth of both sexes, their method is admirable, and\n\
\ highly deserves our imitation.  These are not suffered to taste a grain\n\
\ of oats, except upon certain days, till eighteen years old; nor milk, but\n\
\ very rarely; and in summer they graze two hours in the morning, and as\n\
\ many in the evening, which their parents likewise observe; but the\n\
\ servants are not allowed above half that time, and a great part of their\n\
\ grass is brought home, which they eat at the most convenient hours, when\n\
\ they can be best spared from work.\n\
\ \n\
\ Temperance, industry, exercise, and cleanliness, are the lessons equally\n\
\ enjoined to the young ones of both sexes: and my master thought it\n\
\ monstrous in us, to give the females a different kind of education from\n\
\ the males, except in some articles of domestic management; whereby, as he\n\
\ truly observed, one half of our natives were good for nothing but\n\
\ bringing children into the world; and to trust the care of our children\n\
\ to such useless animals, he said, was yet a greater instance of\n\
\ brutality.\n\
\ \n\
\ But the _Houyhnhnms_ train up their youth to strength, speed, and\n\
\ hardiness, by exercising them in running races up and down steep hills,\n\
\ and over hard stony grounds; and when they are all in a sweat, they are\n\
\ ordered to leap over head and ears into a pond or river.  Four times a\n\
\ year the youth of a certain district meet to show their proficiency in\n\
\ running and leaping, and other feats of strength and agility; where the\n\
\ victor is rewarded with a song in his or her praise.  On this festival,\n\
\ the servants drive a herd of _Yahoos_ into the field, laden with hay, and\n\
\ oats, and milk, for a repast to the _Houyhnhnms_; after which, these\n\
\ brutes are immediately driven back again, for fear of being noisome to\n\
\ the assembly.\n\
\ \n\
\ Every fourth year, at the vernal equinox, there is a representative\n\
\ council of the whole nation, which meets in a plain about twenty miles\n\
\ from our house, and continues about five or six days.  Here they inquire\n\
\ into the state and condition of the several districts; whether they\n\
\ abound or be deficient in hay or oats, or cows, or _Yahoos_; and wherever\n\
\ there is any want (which is but seldom) it is immediately supplied by\n\
\ unanimous consent and contribution.  Here likewise the regulation of\n\
\ children is settled: as for instance, if a _Houyhnhnm_ has two males, he\n\
\ changes one of them with another that has two females; and when a child\n\
\ has been lost by any casualty, where the mother is past breeding, it is\n\
\ determined what family in the district shall breed another to supply the\n\
\ loss.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER IX.\n\
\ \n\
\ \n\
\ A grand debate at the general assembly of the _Houyhnhnms_, and how it\n\
\ was determined.  The learning of the _Houyhnhnms_.  Their buildings.\n\
\ Their manner of burials.  The defectiveness of their language.\n\
\ \n\
\ One of these grand assemblies was held in my time, about three months\n\
\ before my departure, whither my master went as the representative of our\n\
\ district.  In this council was resumed their old debate, and indeed the\n\
\ only debate that ever happened in their country; whereof my master, after\n\
\ his return, give me a very particular account.\n\
\ \n\
\ The question to be debated was, \"whether the _Yahoos_ should be\n\
\ exterminated from the face of the earth?\"  One of the members for the\n\
\ affirmative offered several arguments of great strength and weight,\n\
\ alleging, \"that as the _Yahoos_ were the most filthy, noisome, and\n\
\ deformed animals which nature ever produced, so they were the most\n\
\ restive and indocible, mischievous and malicious; they would privately\n\
\ suck the teats of the _Houyhnhnms'_ cows, kill and devour their cats,\n\
\ trample down their oats and grass, if they were not continually watched,\n\
\ and commit a thousand other extravagancies.\"  He took notice of a general\n\
\ tradition, \"that _Yahoos_ had not been always in their country; but that\n\
\ many ages ago, two of these brutes appeared together upon a mountain;\n\
\ whether produced by the heat of the sun upon corrupted mud and slime, or\n\
\ from the ooze and froth of the sea, was never known; that these _Yahoos_\n\
\ engendered, and their brood, in a short time, grew so numerous as to\n\
\ overrun and infest the whole nation; that the _Houyhnhnms_, to get rid of\n\
\ this evil, made a general hunting, and at last enclosed the whole herd;\n\
\ and destroying the elder, every _Houyhnhnm_ kept two young ones in a\n\
\ kennel, and brought them to such a degree of tameness, as an animal, so\n\
\ savage by nature, can be capable of acquiring, using them for draught and\n\
\ carriage; that there seemed to be much truth in this tradition, and that\n\
\ those creatures could not be _yinhniamshy_ (or _aborigines_ of the land),\n\
\ because of the violent hatred the _Houyhnhnms_, as well as all other\n\
\ animals, bore them, which, although their evil disposition sufficiently\n\
\ deserved, could never have arrived at so high a degree if they had been\n\
\ _aborigines_, or else they would have long since been rooted out; that\n\
\ the inhabitants, taking a fancy to use the service of the _Yahoos_, had,\n\
\ very imprudently, neglected to cultivate the breed of asses, which are a\n\
\ comely animal, easily kept, more tame and orderly, without any offensive\n\
\ smell, strong enough for labour, although they yield to the other in\n\
\ agility of body, and if their braying be no agreeable sound, it is far\n\
\ preferable to the horrible howlings of the _Yahoos_.\"\n\
\ \n\
\ Several others declared their sentiments to the same purpose, when my\n\
\ master proposed an expedient to the assembly, whereof he had indeed\n\
\ borrowed the hint from me.  \"He approved of the tradition mentioned by\n\
\ the honourable member who spoke before, and affirmed, that the two\n\
\ _Yahoos_ said to be seen first among them, had been driven thither over\n\
\ the sea; that coming to land, and being forsaken by their companions,\n\
\ they retired to the mountains, and degenerating by degrees, became in\n\
\ process of time much more savage than those of their own species in the\n\
\ country whence these two originals came.  The reason of this assertion\n\
\ was, that he had now in his possession a certain wonderful _Yahoo_\n\
\ (meaning myself) which most of them had heard of, and many of them had\n\
\ seen.  He then related to them how he first found me; that my body was\n\
\ all covered with an artificial composure of the skins and hairs of other\n\
\ animals; that I spoke in a language of my own, and had thoroughly learned\n\
\ theirs; that I had related to him the accidents which brought me thither;\n\
\ that when he saw me without my covering, I was an exact _Yahoo_ in every\n\
\ part, only of a whiter colour, less hairy, and with shorter claws.  He\n\
\ added, how I had endeavoured to persuade him, that in my own and other\n\
\ countries, the _Yahoos_ acted as the governing, rational animal, and held\n\
\ the _Houyhnhnms_ in servitude; that he observed in me all the qualities\n\
\ of a _Yahoo_, only a little more civilized by some tincture of reason,\n\
\ which, however, was in a degree as far inferior to the _Houyhnhnm_ race,\n\
\ as the _Yahoos_ of their country were to me; that, among other things, I\n\
\ mentioned a custom we had of castrating _Houyhnhnms_ when they were\n\
\ young, in order to render them tame; that the operation was easy and\n\
\ safe; that it was no shame to learn wisdom from brutes, as industry is\n\
\ taught by the ant, and building by the swallow (for so I translate the\n\
\ word _lyhannh_, although it be a much larger fowl); that this invention\n\
\ might be practised upon the younger _Yahoos_ here, which besides\n\
\ rendering them tractable and fitter for use, would in an age put an end\n\
\ to the whole species, without destroying life; that in the mean time the\n\
\ _Houyhnhnms_ should be exhorted to cultivate the breed of asses, which,\n\
\ as they are in all respects more valuable brutes, so they have this\n\
\ advantage, to be fit for service at five years old, which the others are\n\
\ not till twelve.\"\n\
\ \n\
\ This was all my master thought fit to tell me, at that time, of what\n\
\ passed in the grand council.  But he was pleased to conceal one\n\
\ particular, which related personally to myself, whereof I soon felt the\n\
\ unhappy effect, as the reader will know in its proper place, and whence I\n\
\ date all the succeeding misfortunes of my life.\n\
\ \n\
\ The _Houyhnhnms_ have no letters, and consequently their knowledge is all\n\
\ traditional.  But there happening few events of any moment among a people\n\
\ so well united, naturally disposed to every virtue, wholly governed by\n\
\ reason, and cut off from all commerce with other nations, the historical\n\
\ part is easily preserved without burdening their memories.  I have\n\
\ already observed that they are subject to no diseases, and therefore can\n\
\ have no need of physicians.  However, they have excellent medicines,\n\
\ composed of herbs, to cure accidental bruises and cuts in the pastern or\n\
\ frog of the foot, by sharp stones, as well as other maims and hurts in\n\
\ the several parts of the body.\n\
\ \n\
\ They calculate the year by the revolution of the sun and moon, but use no\n\
\ subdivisions into weeks.  They are well enough acquainted with the\n\
\ motions of those two luminaries, and understand the nature of eclipses;\n\
\ and this is the utmost progress of their astronomy.\n\
\ \n\
\ In poetry, they must be allowed to excel all other mortals; wherein the\n\
\ justness of their similes, and the minuteness as well as exactness of\n\
\ their descriptions, are indeed inimitable.  Their verses abound very much\n\
\ in both of these, and usually contain either some exalted notions of\n\
\ friendship and benevolence or the praises of those who were victors in\n\
\ races and other bodily exercises.  Their buildings, although very rude\n\
\ and simple, are not inconvenient, but well contrived to defend them from\n\
\ all injuries of cold and heat.  They have a kind of tree, which at forty\n\
\ years old loosens in the root, and falls with the first storm: it grows\n\
\ very straight, and being pointed like stakes with a sharp stone (for the\n\
\ _Houyhnhnms_ know not the use of iron), they stick them erect in the\n\
\ ground, about ten inches asunder, and then weave in oat straw, or\n\
\ sometimes wattles, between them.  The roof is made after the same manner,\n\
\ and so are the doors.\n\
\ \n\
\ The _Houyhnhnms_ use the hollow part, between the pastern and the hoof of\n\
\ their fore-foot, as we do our hands, and this with greater dexterity than\n\
\ I could at first imagine.  I have seen a white mare of our family thread\n\
\ a needle (which I lent her on purpose) with that joint.  They milk their\n\
\ cows, reap their oats, and do all the work which requires hands, in the\n\
\ same manner.  They have a kind of hard flints, which, by grinding against\n\
\ other stones, they form into instruments, that serve instead of wedges,\n\
\ axes, and hammers.  With tools made of these flints, they likewise cut\n\
\ their hay, and reap their oats, which there grow naturally in several\n\
\ fields; the _Yahoos_ draw home the sheaves in carriages, and the servants\n\
\ tread them in certain covered huts to get out the grain, which is kept in\n\
\ stores.  They make a rude kind of earthen and wooden vessels, and bake\n\
\ the former in the sun.\n\
\ \n\
\ If they can avoid casualties, they die only of old age, and are buried in\n\
\ the obscurest places that can be found, their friends and relations\n\
\ expressing neither joy nor grief at their departure; nor does the dying\n\
\ person discover the least regret that he is leaving the world, any more\n\
\ than if he were upon returning home from a visit to one of his\n\
\ neighbours.  I remember my master having once made an appointment with a\n\
\ friend and his family to come to his house, upon some affair of\n\
\ importance: on the day fixed, the mistress and her two children came very\n\
\ late; she made two excuses, first for her husband, who, as she said,\n\
\ happened that very morning to _shnuwnh_.  The word is strongly expressive\n\
\ in their language, but not easily rendered into English; it signifies,\n\
\ \"to retire to his first mother.\"  Her excuse for not coming sooner, was,\n\
\ that her husband dying late in the morning, she was a good while\n\
\ consulting her servants about a convenient place where his body should be\n\
\ laid; and I observed, she behaved herself at our house as cheerfully as\n\
\ the rest.  She died about three months after.\n\
\ \n\
\ They live generally to seventy, or seventy-five years, very seldom to\n\
\ fourscore.  Some weeks before their death, they feel a gradual decay; but\n\
\ without pain.  During this time they are much visited by their friends,\n\
\ because they cannot go abroad with their usual ease and satisfaction.\n\
\ However, about ten days before their death, which they seldom fail in\n\
\ computing, they return the visits that have been made them by those who\n\
\ are nearest in the neighbourhood, being carried in a convenient sledge\n\
\ drawn by _Yahoos_; which vehicle they use, not only upon this occasion,\n\
\ but when they grow old, upon long journeys, or when they are lamed by any\n\
\ accident: and therefore when the dying _Houyhnhnms_ return those visits,\n\
\ they take a solemn leave of their friends, as if they were going to some\n\
\ remote part of the country, where they designed to pass the rest of their\n\
\ lives.\n\
\ \n\
\ I know not whether it may be worth observing, that the _Houyhnhnms_ have\n\
\ no word in their language to express any thing that is evil, except what\n\
\ they borrow from the deformities or ill qualities of the _Yahoos_.  Thus\n\
\ they denote the folly of a servant, an omission of a child, a stone that\n\
\ cuts their feet, a continuance of foul or unseasonable weather, and the\n\
\ like, by adding to each the epithet of _Yahoo_.  For instance, _hhnm\n\
\ Yahoo_; _whnaholm Yahoo_, _ynlhmndwihlma Yahoo_, and an ill-contrived\n\
\ house _ynholmhnmrohlnw Yahoo_.\n\
\ \n\
\ I could, with great pleasure, enlarge further upon the manners and\n\
\ virtues of this excellent people; but intending in a short time to\n\
\ publish a volume by itself, expressly upon that subject, I refer the\n\
\ reader thither; and, in the mean time, proceed to relate my own sad\n\
\ catastrophe.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER X.\n\
\ \n\
\ \n\
\ The author's economy, and happy life, among the Houyhnhnms.  His great\n\
\ improvement in virtue by conversing with them.  Their conversations.  The\n\
\ author has notice given him by his master, that he must depart from the\n\
\ country.  He falls into a swoon for grief; but submits.  He contrives and\n\
\ finishes a canoe by the help of a fellow-servant, and puts to sea at a\n\
\ venture.\n\
\ \n\
\ I had settled my little economy to my own heart's content.  My master had\n\
\ ordered a room to be made for me, after their manner, about six yards\n\
\ from the house: the sides and floors of which I plastered with clay, and\n\
\ covered with rush-mats of my own contriving.  I had beaten hemp, which\n\
\ there grows wild, and made of it a sort of ticking; this I filled with\n\
\ the feathers of several birds I had taken with springes made of _Yahoos'_\n\
\ hairs, and were excellent food.  I had worked two chairs with my knife,\n\
\ the sorrel nag helping me in the grosser and more laborious part.  When\n\
\ my clothes were worn to rags, I made myself others with the skins of\n\
\ rabbits, and of a certain beautiful animal, about the same size, called\n\
\ _nnuhnoh_, the skin of which is covered with a fine down.  Of these I\n\
\ also made very tolerable stockings.  I soled my shoes with wood, which I\n\
\ cut from a tree, and fitted to the upper-leather; and when this was worn\n\
\ out, I supplied it with the skins of _Yahoos_ dried in the sun.  I often\n\
\ got honey out of hollow trees, which I mingled with water, or ate with my\n\
\ bread.  No man could more verify the truth of these two maxims, \"That\n\
\ nature is very easily satisfied;\" and, \"That necessity is the mother of\n\
\ invention.\"  I enjoyed perfect health of body, and tranquillity of mind;\n\
\ I did not feel the treachery or inconstancy of a friend, nor the injuries\n\
\ of a secret or open enemy.  I had no occasion of bribing, flattering, or\n\
\ pimping, to procure the favour of any great man, or of his minion; I\n\
\ wanted no fence against fraud or oppression: here was neither physician\n\
\ to destroy my body, nor lawyer to ruin my fortune; no informer to watch\n\
\ my words and actions, or forge accusations against me for hire: here were\n\
\ no gibers, censurers, backbiters, pickpockets, highwaymen, housebreakers,\n\
\ attorneys, bawds, buffoons, gamesters, politicians, wits, splenetics,\n\
\ tedious talkers, controvertists, ravishers, murderers, robbers,\n\
\ virtuosos; no leaders, or followers, of party and faction; no encouragers\n\
\ to vice, by seducement or examples; no dungeon, axes, gibbets,\n\
\ whipping-posts, or pillories; no cheating shopkeepers or mechanics; no\n\
\ pride, vanity, or affectation; no fops, bullies, drunkards, strolling\n\
\ whores, or poxes; no ranting, lewd, expensive wives; no stupid, proud\n\
\ pedants; no importunate, overbearing, quarrelsome, noisy, roaring, empty,\n\
\ conceited, swearing companions; no scoundrels raised from the dust upon\n\
\ the merit of their vices, or nobility thrown into it on account of their\n\
\ virtues; no lords, fiddlers, judges, or dancing-masters.\n\
\ \n\
\ I had the favour of being admitted to several _Houyhnhnms_, who came to\n\
\ visit or dine with my master; where his honour graciously suffered me to\n\
\ wait in the room, and listen to their discourse.  Both he and his company\n\
\ would often descend to ask me questions, and receive my answers.  I had\n\
\ also sometimes the honour of attending my master in his visits to others.\n\
\ I never presumed to speak, except in answer to a question; and then I did\n\
\ it with inward regret, because it was a loss of so much time for\n\
\ improving myself; but I was infinitely delighted with the station of an\n\
\ humble auditor in such conversations, where nothing passed but what was\n\
\ useful, expressed in the fewest and most significant words; where, as I\n\
\ have already said, the greatest decency was observed, without the least\n\
\ degree of ceremony; where no person spoke without being pleased himself,\n\
\ and pleasing his companions; where there was no interruption,\n\
\ tediousness, heat, or difference of sentiments.  They have a notion, that\n\
\ when people are met together, a short silence does much improve\n\
\ conversation: this I found to be true; for during those little\n\
\ intermissions of talk, new ideas would arise in their minds, which very\n\
\ much enlivened the discourse.  Their subjects are, generally on\n\
\ friendship and benevolence, on order and economy; sometimes upon the\n\
\ visible operations of nature, or ancient traditions; upon the bounds and\n\
\ limits of virtue; upon the unerring rules of reason, or upon some\n\
\ determinations to be taken at the next great assembly: and often upon the\n\
\ various excellences of poetry.  I may add, without vanity, that my\n\
\ presence often gave them sufficient matter for discourse, because it\n\
\ afforded my master an occasion of letting his friends into the history of\n\
\ me and my country, upon which they were all pleased to descant, in a\n\
\ manner not very advantageous to humankind: and for that reason I shall\n\
\ not repeat what they said; only I may be allowed to observe, that his\n\
\ honour, to my great admiration, appeared to understand the nature of\n\
\ _Yahoos_ much better than myself.  He went through all our vices and\n\
\ follies, and discovered many, which I had never mentioned to him, by only\n\
\ supposing what qualities a _Yahoo_ of their country, with a small\n\
\ proportion of reason, might be capable of exerting; and concluded, with\n\
\ too much probability, \"how vile, as well as miserable, such a creature\n\
\ must be.\"\n\
\ \n\
\ I freely confess, that all the little knowledge I have of any value, was\n\
\ acquired by the lectures I received from my master, and from hearing the\n\
\ discourses of him and his friends; to which I should be prouder to\n\
\ listen, than to dictate to the greatest and wisest assembly in Europe.  I\n\
\ admired the strength, comeliness, and speed of the inhabitants; and such\n\
\ a constellation of virtues, in such amiable persons, produced in me the\n\
\ highest veneration.  At first, indeed, I did not feel that natural awe,\n\
\ which the _Yahoos_ and all other animals bear toward them; but it grew\n\
\ upon me by decrees, much sooner than I imagined, and was mingled with a\n\
\ respectful love and gratitude, that they would condescend to distinguish\n\
\ me from the rest of my species.\n\
\ \n\
\ When I thought of my family, my friends, my countrymen, or the human race\n\
\ in general, I considered them, as they really were, _Yahoos_ in shape and\n\
\ disposition, perhaps a little more civilized, and qualified with the gift\n\
\ of speech; but making no other use of reason, than to improve and\n\
\ multiply those vices whereof their brethren in this country had only the\n\
\ share that nature allotted them.  When I happened to behold the\n\
\ reflection of my own form in a lake or fountain, I turned away my face in\n\
\ horror and detestation of myself, and could better endure the sight of a\n\
\ common _Yahoo_ than of my own person.  By conversing with the\n\
\ _Houyhnhnms_, and looking upon them with delight, I fell to imitate their\n\
\ gait and gesture, which is now grown into a habit; and my friends often\n\
\ tell me, in a blunt way, \"that I trot like a horse;\" which, however, I\n\
\ take for a great compliment.  Neither shall I disown, that in speaking I\n\
\ am apt to fall into the voice and manner of the _Houyhnhnms_, and hear\n\
\ myself ridiculed on that account, without the least mortification.\n\
\ \n\
\ In the midst of all this happiness, and when I looked upon myself to be\n\
\ fully settled for life, my master sent for me one morning a little\n\
\ earlier than his usual hour.  I observed by his countenance that he was\n\
\ in some perplexity, and at a loss how to begin what he had to speak.\n\
\ After a short silence, he told me, \"he did not know how I would take what\n\
\ he was going to say: that in the last general assembly, when the affair\n\
\ of the _Yahoos_ was entered upon, the representatives had taken offence\n\
\ at his keeping a _Yahoo_ (meaning myself) in his family, more like a\n\
\ _Houyhnhnm_ than a brute animal; that he was known frequently to converse\n\
\ with me, as if he could receive some advantage or pleasure in my company;\n\
\ that such a practice was not agreeable to reason or nature, or a thing\n\
\ ever heard of before among them; the assembly did therefore exhort him\n\
\ either to employ me like the rest of my species, or command me to swim\n\
\ back to the place whence I came: that the first of these expedients was\n\
\ utterly rejected by all the _Houyhnhnms_ who had ever seen me at his\n\
\ house or their own; for they alleged, that because I had some rudiments\n\
\ of reason, added to the natural pravity of those animals, it was to be\n\
\ feared I might be able to seduce them into the woody and mountainous\n\
\ parts of the country, and bring them in troops by night to destroy the\n\
\ _Houyhnhnms'_ cattle, as being naturally of the ravenous kind, and averse\n\
\ from labour.\"\n\
\ \n\
\ My master added, \"that he was daily pressed by the _Houyhnhnms_ of the\n\
\ neighbourhood to have the assembly's exhortation executed, which he could\n\
\ not put off much longer.  He doubted it would be impossible for me to\n\
\ swim to another country; and therefore wished I would contrive some sort\n\
\ of vehicle, resembling those I had described to him, that might carry me\n\
\ on the sea; in which work I should have the assistance of his own\n\
\ servants, as well as those of his neighbours.\"  He concluded, \"that for\n\
\ his own part, he could have been content to keep me in his service as\n\
\ long as I lived; because he found I had cured myself of some bad habits\n\
\ and dispositions, by endeavouring, as far as my inferior nature was\n\
\ capable, to imitate the _Houyhnhnms_.\"\n\
\ \n\
\ I should here observe to the reader, that a decree of the general\n\
\ assembly in this country is expressed by the word _hnhloayn_, which\n\
\ signifies an exhortation, as near as I can render it; for they have no\n\
\ conception how a rational creature can be compelled, but only advised, or\n\
\ exhorted; because no person can disobey reason, without giving up his\n\
\ claim to be a rational creature.\n\
\ \n\
\ I was struck with the utmost grief and despair at my master's discourse;\n\
\ and being unable to support the agonies I was under, I fell into a swoon\n\
\ at his feet.  When I came to myself, he told me \"that he concluded I had\n\
\ been dead;\" for these people are subject to no such imbecilities of\n\
\ nature.  I answered in a faint voice, \"that death would have been too\n\
\ great a happiness; that although I could not blame the assembly's\n\
\ exhortation, or the urgency of his friends; yet, in my weak and corrupt\n\
\ judgment, I thought it might consist with reason to have been less\n\
\ rigorous; that I could not swim a league, and probably the nearest land\n\
\ to theirs might be distant above a hundred: that many materials,\n\
\ necessary for making a small vessel to carry me off, were wholly wanting\n\
\ in this country; which, however, I would attempt, in obedience and\n\
\ gratitude to his honour, although I concluded the thing to be impossible,\n\
\ and therefore looked on myself as already devoted to destruction; that\n\
\ the certain prospect of an unnatural death was the least of my evils;\n\
\ for, supposing I should escape with life by some strange adventure, how\n\
\ could I think with temper of passing my days among _Yahoos_, and\n\
\ relapsing into my old corruptions, for want of examples to lead and keep\n\
\ me within the paths of virtue? that I knew too well upon what solid\n\
\ reasons all the determinations of the wise _Houyhnhnms_ were founded, not\n\
\ to be shaken by arguments of mine, a miserable _Yahoo_; and therefore,\n\
\ after presenting him with my humble thanks for the offer of his servants'\n\
\ assistance in making a vessel, and desiring a reasonable time for so\n\
\ difficult a work, I told him I would endeavour to preserve a wretched\n\
\ being; and if ever I returned to England, was not without hopes of being\n\
\ useful to my own species, by celebrating the praises of the renowned\n\
\ _Houyhnhnms_, and proposing their virtues to the imitation of mankind.\"\n\
\ \n\
\ My master, in a few words, made me a very gracious reply; allowed me the\n\
\ space of two months to finish my boat; and ordered the sorrel nag, my\n\
\ fellow-servant (for so, at this distance, I may presume to call him), to\n\
\ follow my instruction; because I told my master, \"that his help would be\n\
\ sufficient, and I knew he had a tenderness for me.\"\n\
\ \n\
\ In his company, my first business was to go to that part of the coast\n\
\ where my rebellious crew had ordered me to be set on shore.  I got upon a\n\
\ height, and looking on every side into the sea; fancied I saw a small\n\
\ island toward the north-east.  I took out my pocket glass, and could then\n\
\ clearly distinguish it above five leagues off, as I computed; but it\n\
\ appeared to the sorrel nag to be only a blue cloud: for as he had no\n\
\ conception of any country beside his own, so he could not be as expert in\n\
\ distinguishing remote objects at sea, as we who so much converse in that\n\
\ element.\n\
\ \n\
\ After I had discovered this island, I considered no further; but resolved\n\
\ it should if possible, be the first place of my banishment, leaving the\n\
\ consequence to fortune.\n\
\ \n\
\ I returned home, and consulting with the sorrel nag, we went into a copse\n\
\ at some distance, where I with my knife, and he with a sharp flint,\n\
\ fastened very artificially after their manner, to a wooden handle, cut\n\
\ down several oak wattles, about the thickness of a walking-staff, and\n\
\ some larger pieces.  But I shall not trouble the reader with a particular\n\
\ description of my own mechanics; let it suffice to say, that in six weeks\n\
\ time with the help of the sorrel nag, who performed the parts that\n\
\ required most labour, I finished a sort of Indian canoe, but much larger,\n\
\ covering it with the skins of _Yahoos_, well stitched together with\n\
\ hempen threads of my own making.  My sail was likewise composed of the\n\
\ skins of the same animal; but I made use of the youngest I could get, the\n\
\ older being too tough and thick; and I likewise provided myself with four\n\
\ paddles.  I laid in a stock of boiled flesh, of rabbits and fowls, and\n\
\ took with me two vessels, one filled with milk and the other with water.\n\
\ \n\
\ I tried my canoe in a large pond, near my master's house, and then\n\
\ corrected in it what was amiss; stopping all the chinks with _Yahoos'_\n\
\ tallow, till I found it staunch, and able to bear me and my freight; and,\n\
\ when it was as complete as I could possibly make it, I had it drawn on a\n\
\ carriage very gently by _Yahoos_ to the sea-side, under the conduct of\n\
\ the sorrel nag and another servant.\n\
\ \n\
\ When all was ready, and the day came for my departure, I took leave of my\n\
\ master and lady and the whole family, my eyes flowing with tears, and my\n\
\ heart quite sunk with grief.  But his honour, out of curiosity, and,\n\
\ perhaps, (if I may speak without vanity,) partly out of kindness, was\n\
\ determined to see me in my canoe, and got several of his neighbouring\n\
\ friends to accompany him.  I was forced to wait above an hour for the\n\
\ tide; and then observing the wind very fortunately bearing toward the\n\
\ island to which I intended to steer my course, I took a second leave of\n\
\ my master: but as I was going to prostrate myself to kiss his hoof, he\n\
\ did me the honour to raise it gently to my mouth.  I am not ignorant how\n\
\ much I have been censured for mentioning this last particular.\n\
\ Detractors are pleased to think it improbable, that so illustrious a\n\
\ person should descend to give so great a mark of distinction to a\n\
\ creature so inferior as I.  Neither have I forgotten how apt some\n\
\ travellers are to boast of extraordinary favours they have received.\n\
\ But, if these censurers were better acquainted with the noble and\n\
\ courteous disposition of the _Houyhnhnms_, they would soon change their\n\
\ opinion.\n\
\ \n\
\ I paid my respects to the rest of the _Houyhnhnms_ in his honour's\n\
\ company; then getting into my canoe, I pushed off from shore.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER XI.\n\
\ \n\
\ \n\
\ The author's dangerous voyage.  He arrives at New Holland, hoping to\n\
\ settle there.  Is wounded with an arrow by one of the natives.  Is seized\n\
\ and carried by force into a Portuguese ship.  The great civilities of the\n\
\ captain.  The author arrives at England.\n\
\ \n\
\ I began this desperate voyage on February 15, 1714-15, at nine o'clock in\n\
\ the morning.  The wind was very favourable; however, I made use at first\n\
\ only of my paddles; but considering I should soon be weary, and that the\n\
\ wind might chop about, I ventured to set up my little sail; and thus,\n\
\ with the help of the tide, I went at the rate of a league and a half an\n\
\ hour, as near as I could guess.  My master and his friends continued on\n\
\ the shore till I was almost out of sight; and I often heard the sorrel\n\
\ nag (who always loved me) crying out, \"_Hnuy illa nyha_, _majah Yahoo_;\"\n\
\ \"Take care of thyself, gentle _Yahoo_.\"\n\
\ \n\
\ My design was, if possible, to discover some small island uninhabited,\n\
\ yet sufficient, by my labour, to furnish me with the necessaries of life,\n\
\ which I would have thought a greater happiness, than to be first minister\n\
\ in the politest court of Europe; so horrible was the idea I conceived of\n\
\ returning to live in the society, and under the government of _Yahoos_.\n\
\ For in such a solitude as I desired, I could at least enjoy my own\n\
\ thoughts, and reflect with delight on the virtues of those inimitable\n\
\ _Houyhnhnms_, without an opportunity of degenerating into the vices and\n\
\ corruptions of my own species.\n\
\ \n\
\ The reader may remember what I related, when my crew conspired against\n\
\ me, and confined me to my cabin; how I continued there several weeks\n\
\ without knowing what course we took; and when I was put ashore in the\n\
\ long-boat, how the sailors told me, with oaths, whether true or false,\n\
\ \"that they knew not in what part of the world we were.\"  However, I did\n\
\ then believe us to be about 10 degrees southward of the Cape of Good\n\
\ Hope, or about 45 degrees southern latitude, as I gathered from some\n\
\ general words I overheard among them, being I supposed to the south-east\n\
\ in their intended voyage to Madagascar.  And although this were little\n\
\ better than conjecture, yet I resolved to steer my course eastward,\n\
\ hoping to reach the south-west coast of New Holland, and perhaps some\n\
\ such island as I desired lying westward of it.  The wind was full west,\n\
\ and by six in the evening I computed I had gone eastward at least\n\
\ eighteen leagues; when I spied a very small island about half a league\n\
\ off, which I soon reached.  It was nothing but a rock, with one creek\n\
\ naturally arched by the force of tempests.  Here I put in my canoe, and\n\
\ climbing a part of the rock, I could plainly discover land to the east,\n\
\ extending from south to north.  I lay all night in my canoe; and\n\
\ repeating my voyage early in the morning, I arrived in seven hours to the\n\
\ south-east point of New Holland.  This confirmed me in the opinion I have\n\
\ long entertained, that the maps and charts place this country at least\n\
\ three degrees more to the east than it really is; which thought I\n\
\ communicated many years ago to my worthy friend, Mr. Herman Moll, and\n\
\ gave him my reasons for it, although he has rather chosen to follow other\n\
\ authors.\n\
\ \n\
\ I saw no inhabitants in the place where I landed, and being unarmed, I\n\
\ was afraid of venturing far into the country.  I found some shellfish on\n\
\ the shore, and ate them raw, not daring to kindle a fire, for fear of\n\
\ being discovered by the natives.  I continued three days feeding on\n\
\ oysters and limpets, to save my own provisions; and I fortunately found a\n\
\ brook of excellent water, which gave me great relief.\n\
\ \n\
\ On the fourth day, venturing out early a little too far, I saw twenty or\n\
\ thirty natives upon a height not above five hundred yards from me.  They\n\
\ were stark naked, men, women, and children, round a fire, as I could\n\
\ discover by the smoke.  One of them spied me, and gave notice to the\n\
\ rest; five of them advanced toward me, leaving the women and children at\n\
\ the fire.  I made what haste I could to the shore, and, getting into my\n\
\ canoe, shoved off: the savages, observing me retreat, ran after me: and\n\
\ before I could get far enough into the sea, discharged an arrow which\n\
\ wounded me deeply on the inside of my left knee: I shall carry the mark\n\
\ to my grave.  I apprehended the arrow might be poisoned, and paddling out\n\
\ of the reach of their darts (being a calm day), I made a shift to suck\n\
\ the wound, and dress it as well as I could.\n\
\ \n\
\ I was at a loss what to do, for I durst not return to the same\n\
\ landing-place, but stood to the north, and was forced to paddle, for the\n\
\ wind, though very gentle, was against me, blowing north-west.  As I was\n\
\ looking about for a secure landing-place, I saw a sail to the\n\
\ north-north-east, which appearing every minute more visible, I was in\n\
\ some doubt whether I should wait for them or not; but at last my\n\
\ detestation of the _Yahoo_ race prevailed: and turning my canoe, I sailed\n\
\ and paddled together to the south, and got into the same creek whence I\n\
\ set out in the morning, choosing rather to trust myself among these\n\
\ barbarians, than live with European _Yahoos_.  I drew up my canoe as\n\
\ close as I could to the shore, and hid myself behind a stone by the\n\
\ little brook, which, as I have already said, was excellent water.\n\
\ \n\
\ The ship came within half a league of this creek, and sent her long boat\n\
\ with vessels to take in fresh water (for the place, it seems, was very\n\
\ well known); but I did not observe it, till the boat was almost on shore;\n\
\ and it was too late to seek another hiding-place.  The seamen at their\n\
\ landing observed my canoe, and rummaging it all over, easily conjectured\n\
\ that the owner could not be far off.  Four of them, well armed, searched\n\
\ every cranny and lurking-hole, till at last they found me flat on my face\n\
\ behind the stone.  They gazed awhile in admiration at my strange uncouth\n\
\ dress; my coat made of skins, my wooden-soled shoes, and my furred\n\
\ stockings; whence, however, they concluded, I was not a native of the\n\
\ place, who all go naked.  One of the seamen, in Portuguese, bid me rise,\n\
\ and asked who I was.  I understood that language very well, and getting\n\
\ upon my feet, said, \"I was a poor _Yahoo_ banished from the _Houyhnhnms_,\n\
\ and desired they would please to let me depart.\"  They admired to hear me\n\
\ answer them in their own tongue, and saw by my complexion I must be a\n\
\ European; but were at a loss to know what I meant by _Yahoos_ and\n\
\ _Houyhnhnms_; and at the same time fell a-laughing at my strange tone in\n\
\ speaking, which resembled the neighing of a horse.  I trembled all the\n\
\ while betwixt fear and hatred.  I again desired leave to depart, and was\n\
\ gently moving to my canoe; but they laid hold of me, desiring to know,\n\
\ \"what country I was of? whence I came?\" with many other questions.  I\n\
\ told them \"I was born in England, whence I came about five years ago, and\n\
\ then their country and ours were at peace.  I therefore hoped they would\n\
\ not treat me as an enemy, since I meant them no harm, but was a poor\n\
\ _Yahoo_ seeking some desolate place where to pass the remainder of his\n\
\ unfortunate life.\"\n\
\ \n\
\ When they began to talk, I thought I never heard or saw any thing more\n\
\ unnatural; for it appeared to me as monstrous as if a dog or a cow should\n\
\ speak in England, or a _Yahoo_ in _Houyhnhnmland_.  The honest Portuguese\n\
\ were equally amazed at my strange dress, and the odd manner of delivering\n\
\ my words, which, however, they understood very well.  They spoke to me\n\
\ with great humanity, and said, \"they were sure the captain would carry me\n\
\ _gratis_ to Lisbon, whence I might return to my own country; that two of\n\
\ the seamen would go back to the ship, inform the captain of what they had\n\
\ seen, and receive his orders; in the mean time, unless I would give my\n\
\ solemn oath not to fly, they would secure me by force.  I thought it best\n\
\ to comply with their proposal.  They were very curious to know my story,\n\
\ but I gave them very little satisfaction, and they all conjectured that\n\
\ my misfortunes had impaired my reason.  In two hours the boat, which went\n\
\ laden with vessels of water, returned, with the captain's command to\n\
\ fetch me on board.  I fell on my knees to preserve my liberty; but all\n\
\ was in vain; and the men, having tied me with cords, heaved me into the\n\
\ boat, whence I was taken into the ship, and thence into the captain's\n\
\ cabin.\n\
\ \n\
\ His name was Pedro de Mendez; he was a very courteous and generous\n\
\ person.  He entreated me to give some account of myself, and desired to\n\
\ know what I would eat or drink; said, \"I should be used as well as\n\
\ himself;\" and spoke so many obliging things, that I wondered to find such\n\
\ civilities from a _Yahoo_.  However, I remained silent and sullen; I was\n\
\ ready to faint at the very smell of him and his men.  At last I desired\n\
\ something to eat out of my own canoe; but he ordered me a chicken, and\n\
\ some excellent wine, and then directed that I should be put to bed in a\n\
\ very clean cabin.  I would not undress myself, but lay on the\n\
\ bed-clothes, and in half an hour stole out, when I thought the crew was\n\
\ at dinner, and getting to the side of the ship, was going to leap into\n\
\ the sea, and swim for my life, rather than continue among _Yahoos_.  But\n\
\ one of the seamen prevented me, and having informed the captain, I was\n\
\ chained to my cabin.\n\
\ \n\
\ After dinner, Don Pedro came to me, and desired to know my reason for so\n\
\ desperate an attempt; assured me, \"he only meant to do me all the service\n\
\ he was able;\" and spoke so very movingly, that at last I descended to\n\
\ treat him like an animal which had some little portion of reason.  I gave\n\
\ him a very short relation of my voyage; of the conspiracy against me by\n\
\ my own men; of the country where they set me on shore, and of my five\n\
\ years residence there.  All which he looked upon as if it were a dream or\n\
\ a vision; whereat I took great offence; for I had quite forgot the\n\
\ faculty of lying, so peculiar to _Yahoos_, in all countries where they\n\
\ preside, and, consequently, their disposition of suspecting truth in\n\
\ others of their own species.  I asked him, \"whether it were the custom in\n\
\ his country to say the thing which was not?\"  I assured him, \"I had\n\
\ almost forgot what he meant by falsehood, and if I had lived a thousand\n\
\ years in _Houyhnhnmland_, I should never have heard a lie from the\n\
\ meanest servant; that I was altogether indifferent whether he believed me\n\
\ or not; but, however, in return for his favours, I would give so much\n\
\ allowance to the corruption of his nature, as to answer any objection he\n\
\ would please to make, and then he might easily discover the truth.\"\n\
\ \n\
\ The captain, a wise man, after many endeavours to catch me tripping in\n\
\ some part of my story, at last began to have a better opinion of my\n\
\ veracity.  But he added, \"that since I professed so inviolable an\n\
\ attachment to truth, I must give him my word and honour to bear him\n\
\ company in this voyage, without attempting any thing against my life; or\n\
\ else he would continue me a prisoner till we arrived at Lisbon.\"  I gave\n\
\ him the promise he required; but at the same time protested, \"that I\n\
\ would suffer the greatest hardships, rather than return to live among\n\
\ _Yahoos_.\"\n\
\ \n\
\ Our voyage passed without any considerable accident.  In gratitude to the\n\
\ captain, I sometimes sat with him, at his earnest request, and strove to\n\
\ conceal my antipathy against human kind, although it often broke out;\n\
\ which he suffered to pass without observation.  But the greatest part of\n\
\ the day I confined myself to my cabin, to avoid seeing any of the crew.\n\
\ The captain had often entreated me to strip myself of my savage dress,\n\
\ and offered to lend me the best suit of clothes he had.  This I would not\n\
\ be prevailed on to accept, abhorring to cover myself with any thing that\n\
\ had been on the back of a _Yahoo_.  I only desired he would lend me two\n\
\ clean shirts, which, having been washed since he wore them, I believed\n\
\ would not so much defile me.  These I changed every second day, and\n\
\ washed them myself.\n\
\ \n\
\ We arrived at Lisbon, Nov. 5, 1715.  At our landing, the captain forced\n\
\ me to cover myself with his cloak, to prevent the rabble from crowding\n\
\ about me.  I was conveyed to his own house; and at my earnest request he\n\
\ led me up to the highest room backwards.  I conjured him \"to conceal from\n\
\ all persons what I had told him of the _Houyhnhnms_; because the least\n\
\ hint of such a story would not only draw numbers of people to see me, but\n\
\ probably put me in danger of being imprisoned, or burnt by the\n\
\ Inquisition.\"  The captain persuaded me to accept a suit of clothes newly\n\
\ made; but I would not suffer the tailor to take my measure; however, Don\n\
\ Pedro being almost of my size, they fitted me well enough.  He accoutred\n\
\ me with other necessaries, all new, which I aired for twenty-four hours\n\
\ before I would use them.\n\
\ \n\
\ The captain had no wife, nor above three servants, none of which were\n\
\ suffered to attend at meals; and his whole deportment was so obliging,\n\
\ added to very good human understanding, that I really began to tolerate\n\
\ his company.  He gained so far upon me, that I ventured to look out of\n\
\ the back window.  By degrees I was brought into another room, whence I\n\
\ peeped into the street, but drew my head back in a fright.  In a week's\n\
\ time he seduced me down to the door.  I found my terror gradually\n\
\ lessened, but my hatred and contempt seemed to increase.  I was at last\n\
\ bold enough to walk the street in his company, but kept my nose well\n\
\ stopped with rue, or sometimes with tobacco.\n\
\ \n\
\ In ten days, Don Pedro, to whom I had given some account of my domestic\n\
\ affairs, put it upon me, as a matter of honour and conscience, \"that I\n\
\ ought to return to my native country, and live at home with my wife and\n\
\ children.\"  He told me, \"there was an English ship in the port just ready\n\
\ to sail, and he would furnish me with all things necessary.\"  It would be\n\
\ tedious to repeat his arguments, and my contradictions.  He said, \"it was\n\
\ altogether impossible to find such a solitary island as I desired to live\n\
\ in; but I might command in my own house, and pass my time in a manner as\n\
\ recluse as I pleased.\"\n\
\ \n\
\ I complied at last, finding I could not do better.  I left Lisbon the\n\
\ 24th day of November, in an English merchantman, but who was the master I\n\
\ never inquired.  Don Pedro accompanied me to the ship, and lent me twenty\n\
\ pounds.  He took kind leave of me, and embraced me at parting, which I\n\
\ bore as well as I could.  During this last voyage I had no commerce with\n\
\ the master or any of his men; but, pretending I was sick, kept close in\n\
\ my cabin.  On the fifth of December, 1715, we cast anchor in the Downs,\n\
\ about nine in the morning, and at three in the afternoon I got safe to my\n\
\ house at Rotherhith. {546}\n\
\ \n\
\ My wife and family received me with great surprise and joy, because they\n\
\ concluded me certainly dead; but I must freely confess the sight of them\n\
\ filled me only with hatred, disgust, and contempt; and the more, by\n\
\ reflecting on the near alliance I had to them.  For although, since my\n\
\ unfortunate exile from the _Houyhnhnm_ country, I had compelled myself to\n\
\ tolerate the sight of _Yahoos_, and to converse with Don Pedro de Mendez,\n\
\ yet my memory and imagination were perpetually filled with the virtues\n\
\ and ideas of those exalted _Houyhnhnms_.  And when I began to consider\n\
\ that, by copulating with one of the _Yahoo_ species I had become a parent\n\
\ of more, it struck me with the utmost shame, confusion, and horror.\n\
\ \n\
\ As soon as I entered the house, my wife took me in her arms, and kissed\n\
\ me; at which, having not been used to the touch of that odious animal for\n\
\ so many years, I fell into a swoon for almost an hour.  At the time I am\n\
\ writing, it is five years since my last return to England.  During the\n\
\ first year, I could not endure my wife or children in my presence; the\n\
\ very smell of them was intolerable; much less could I suffer them to eat\n\
\ in the same room.  To this hour they dare not presume to touch my bread,\n\
\ or drink out of the same cup, neither was I ever able to let one of them\n\
\ take me by the hand.  The first money I laid out was to buy two young\n\
\ stone-horses, which I keep in a good stable; and next to them, the groom\n\
\ is my greatest favourite, for I feel my spirits revived by the smell he\n\
\ contracts in the stable.  My horses understand me tolerably well; I\n\
\ converse with them at least four hours every day.  They are strangers to\n\
\ bridle or saddle; they live in great amity with me and friendship to each\n\
\ other.\n\
\ \n\
\ \n\
\ \n\
\ CHAPTER XII.\n\
\ \n\
\ \n\
\ The author's veracity.  His design in publishing this work.  His censure\n\
\ of those travellers who swerve from the truth.  The author clears himself\n\
\ from any sinister ends in writing.  An objection answered.  The method of\n\
\ planting colonies.  His native country commended.  The right of the crown\n\
\ to those countries described by the author is justified.  The difficulty\n\
\ of conquering them.  The author takes his last leave of the reader;\n\
\ proposes his manner of living for the future; gives good advice, and\n\
\ concludes.\n\
\ \n\
\ Thus, gentle reader, I have given thee a faithful history of my travels\n\
\ for sixteen years and above seven months: wherein I have not been so\n\
\ studious of ornament as of truth.  I could, perhaps, like others, have\n\
\ astonished thee with strange improbable tales; but I rather chose to\n\
\ relate plain matter of fact, in the simplest manner and style; because my\n\
\ principal design was to inform, and not to amuse thee.\n\
\ \n\
\ It is easy for us who travel into remote countries, which are seldom\n\
\ visited by Englishmen or other Europeans, to form descriptions of\n\
\ wonderful animals both at sea and land.  Whereas a traveller's chief aim\n\
\ should be to make men wiser and better, and to improve their minds by the\n\
\ bad, as well as good, example of what they deliver concerning foreign\n\
\ places.\n\
\ \n\
\ I could heartily wish a law was enacted, that every traveller, before he\n\
\ were permitted to publish his voyages, should be obliged to make oath\n\
\ before the Lord High Chancellor, that all he intended to print was\n\
\ absolutely true to the best of his knowledge; for then the world would no\n\
\ longer be deceived, as it usually is, while some writers, to make their\n\
\ works pass the better upon the public, impose the grossest falsities on\n\
\ the unwary reader.  I have perused several books of travels with great\n\
\ delight in my younger days; but having since gone over most parts of the\n\
\ globe, and been able to contradict many fabulous accounts from my own\n\
\ observation, it has given me a great disgust against this part of\n\
\ reading, and some indignation to see the credulity of mankind so\n\
\ impudently abused.  Therefore, since my acquaintance were pleased to\n\
\ think my poor endeavours might not be unacceptable to my country, I\n\
\ imposed on myself, as a maxim never to be swerved from, that I would\n\
\ strictly adhere to truth; neither indeed can I be ever under the least\n\
\ temptation to vary from it, while I retain in my mind the lectures and\n\
\ example of my noble master and the other illustrious _Houyhnhnms_ of whom\n\
\ I had so long the honour to be an humble hearer.\n\
\ \n\
\     _--Nec si miserum Fortuna Sinonem_\n\
\     _Finxit_, _vanum etiam_, _mendacemque improba finget_.\n\
\ \n\
\ I know very well, how little reputation is to be got by writings which\n\
\ require neither genius nor learning, nor indeed any other talent, except\n\
\ a good memory, or an exact journal.  I know likewise, that writers of\n\
\ travels, like dictionary-makers, are sunk into oblivion by the weight and\n\
\ bulk of those who come last, and therefore lie uppermost.  And it is\n\
\ highly probable, that such travellers, who shall hereafter visit the\n\
\ countries described in this work of mine, may, by detecting my errors (if\n\
\ there be any), and adding many new discoveries of their own, justle me\n\
\ out of vogue, and stand in my place, making the world forget that ever I\n\
\ was an author.  This indeed would be too great a mortification, if I\n\
\ wrote for fame: but as my sole intention was the public good, I cannot be\n\
\ altogether disappointed.  For who can read of the virtues I have\n\
\ mentioned in the glorious _Houyhnhnms_, without being ashamed of his own\n\
\ vices, when he considers himself as the reasoning, governing animal of\n\
\ his country?  I shall say nothing of those remote nations where _Yahoos_\n\
\ preside; among which the least corrupted are the _Brobdingnagians_; whose\n\
\ wise maxims in morality and government it would be our happiness to\n\
\ observe.  But I forbear descanting further, and rather leave the\n\
\ judicious reader to his own remarks and application.\n\
\ \n\
\ I am not a little pleased that this work of mine can possibly meet with\n\
\ no censurers: for what objections can be made against a writer, who\n\
\ relates only plain facts, that happened in such distant countries, where\n\
\ we have not the least interest, with respect either to trade or\n\
\ negotiations?  I have carefully avoided every fault with which common\n\
\ writers of travels are often too justly charged.  Besides, I meddle not\n\
\ the least with any party, but write without passion, prejudice, or\n\
\ ill-will against any man, or number of men, whatsoever.  I write for the\n\
\ noblest end, to inform and instruct mankind; over whom I may, without\n\
\ breach of modesty, pretend to some superiority, from the advantages I\n\
\ received by conversing so long among the most accomplished _Houyhnhnms_.\n\
\ I write without any view to profit or praise.  I never suffer a word to\n\
\ pass that may look like reflection, or possibly give the least offence,\n\
\ even to those who are most ready to take it.  So that I hope I may with\n\
\ justice pronounce myself an author perfectly blameless; against whom the\n\
\ tribes of Answerers, Considerers, Observers, Reflectors, Detectors,\n\
\ Remarkers, will never be able to find matter for exercising their\n\
\ talents.\n\
\ \n\
\ I confess, it was whispered to me, \"that I was bound in duty, as a\n\
\ subject of England, to have given in a memorial to a secretary of state\n\
\ at my first coming over; because, whatever lands are discovered by a\n\
\ subject belong to the crown.\"  But I doubt whether our conquests in the\n\
\ countries I treat of would be as easy as those of Ferdinando Cortez over\n\
\ the naked Americans.  The _Lilliputians_, I think, are hardly worth the\n\
\ charge of a fleet and army to reduce them; and I question whether it\n\
\ might be prudent or safe to attempt the _Brobdingnagians_; or whether an\n\
\ English army would be much at their ease with the Flying Island over\n\
\ their heads.  The _Houyhnhnms_ indeed appear not to be so well prepared\n\
\ for war, a science to which they are perfect strangers, and especially\n\
\ against missive weapons.  However, supposing myself to be a minister of\n\
\ state, I could never give my advice for invading them.  Their prudence,\n\
\ unanimity, unacquaintedness with fear, and their love of their country,\n\
\ would amply supply all defects in the military art.  Imagine twenty\n\
\ thousand of them breaking into the midst of an European army, confounding\n\
\ the ranks, overturning the carriages, battering the warriors' faces into\n\
\ mummy by terrible yerks from their hinder hoofs; for they would well\n\
\ deserve the character given to Augustus, _Recalcitrat undique tutus_.\n\
\ But, instead of proposals for conquering that magnanimous nation, I\n\
\ rather wish they were in a capacity, or disposition, to send a sufficient\n\
\ number of their inhabitants for civilizing Europe, by teaching us the\n\
\ first principles of honour, justice, truth, temperance, public spirit,\n\
\ fortitude, chastity, friendship, benevolence, and fidelity.  The names of\n\
\ all which virtues are still retained among us in most languages, and are\n\
\ to be met with in modern, as well as ancient authors; which I am able to\n\
\ assert from my own small reading.\n\
\ \n\
\ But I had another reason, which made me less forward to enlarge his\n\
\ majesty's dominions by my discoveries.  To say the truth, I had conceived\n\
\ a few scruples with relation to the distributive justice of princes upon\n\
\ those occasions.  For instance, a crew of pirates are driven by a storm\n\
\ they know not whither; at length a boy discovers land from the topmast;\n\
\ they go on shore to rob and plunder, they see a harmless people, are\n\
\ entertained with kindness; they give the country a new name; they take\n\
\ formal possession of it for their king; they set up a rotten plank, or a\n\
\ stone, for a memorial; they murder two or three dozen of the natives,\n\
\ bring away a couple more, by force, for a sample; return home, and get\n\
\ their pardon.  Here commences a new dominion acquired with a title by\n\
\ divine right.  Ships are sent with the first opportunity; the natives\n\
\ driven out or destroyed; their princes tortured to discover their gold; a\n\
\ free license given to all acts of inhumanity and lust, the earth reeking\n\
\ with the blood of its inhabitants: and this execrable crew of butchers,\n\
\ employed in so pious an expedition, is a modern colony, sent to convert\n\
\ and civilize an idolatrous and barbarous people!\n\
\ \n\
\ But this description, I confess, does by no means affect the British\n\
\ nation, who may be an example to the whole world for their wisdom, care,\n\
\ and justice in planting colonies; their liberal endowments for the\n\
\ advancement of religion and learning; their choice of devout and able\n\
\ pastors to propagate Christianity; their caution in stocking their\n\
\ provinces with people of sober lives and conversations from this the\n\
\ mother kingdom; their strict regard to the distribution of justice, in\n\
\ supplying the civil administration through all their colonies with\n\
\ officers of the greatest abilities, utter strangers to corruption; and,\n\
\ to crown all, by sending the most vigilant and virtuous governors, who\n\
\ have no other views than the happiness of the people over whom they\n\
\ preside, and the honour of the king their master.\n\
\ \n\
\ But as those countries which I have described do not appear to have any\n\
\ desire of being conquered and enslaved, murdered or driven out by\n\
\ colonies, nor abound either in gold, silver, sugar, or tobacco, I did\n\
\ humbly conceive, they were by no means proper objects of our zeal, our\n\
\ valour, or our interest.  However, if those whom it more concerns think\n\
\ fit to be of another opinion, I am ready to depose, when I shall be\n\
\ lawfully called, that no European did ever visit those countries before\n\
\ me.  I mean, if the inhabitants ought to be believed, unless a dispute\n\
\ may arise concerning the two _Yahoos_, said to have been seen many years\n\
\ ago upon a mountain in _Houyhnhnmland_.\n\
\ \n\
\ But, as to the formality of taking possession in my sovereign's name, it\n\
\ never came once into my thoughts; and if it had, yet, as my affairs then\n\
\ stood, I should perhaps, in point of prudence and self-preservation, have\n\
\ put it off to a better opportunity.\n\
\ \n\
\ Having thus answered the only objection that can ever be raised against\n\
\ me as a traveller, I here take a final leave of all my courteous readers,\n\
\ and return to enjoy my own speculations in my little garden at Redriff;\n\
\ to apply those excellent lessons of virtue which I learned among the\n\
\ _Houyhnhnms_; to instruct the _Yahoos_ of my own family, is far as I\n\
\ shall find them docible animals; to behold my figure often in a glass,\n\
\ and thus, if possible, habituate myself by time to tolerate the sight of\n\
\ a human creature; to lament the brutality to _Houyhnhnms_ in my own\n\
\ country, but always treat their persons with respect, for the sake of my\n\
\ noble master, his family, his friends, and the whole _Houyhnhnm_ race,\n\
\ whom these of ours have the honour to resemble in all their lineaments,\n\
\ however their intellectuals came to degenerate.\n\
\ \n\
\ I began last week to permit my wife to sit at dinner with me, at the\n\
\ farthest end of a long table; and to answer (but with the utmost brevity)\n\
\ the few questions I asked her.  Yet, the smell of a _Yahoo_ continuing\n\
\ very offensive, I always keep my nose well stopped with rue, lavender, or\n\
\ tobacco leaves.  And, although it be hard for a man late in life to\n\
\ remove old habits, I am not altogether out of hopes, in some time, to\n\
\ suffer a neighbour _Yahoo_ in my company, without the apprehensions I am\n\
\ yet under of his teeth or his claws.\n\
\ \n\
\ My reconcilement to the _Yahoo_ kind in general might not be so\n\
\ difficult, if they would be content with those vices and follies only\n\
\ which nature has entitled them to.  I am not in the least provoked at the\n\
\ sight of a lawyer, a pickpocket, a colonel, a fool, a lord, a gamester, a\n\
\ politician, a whoremonger, a physician, an evidence, a suborner, an\n\
\ attorney, a traitor, or the like; this is all according to the due course\n\
\ of things: but when I behold a lump of deformity and diseases, both in\n\
\ body and mind, smitten with pride, it immediately breaks all the measures\n\
\ of my patience; neither shall I be ever able to comprehend how such an\n\
\ animal, and such a vice, could tally together.  The wise and virtuous\n\
\ _Houyhnhnms_, who abound in all excellences that can adorn a rational\n\
\ creature, have no name for this vice in their language, which has no\n\
\ terms to express any thing that is evil, except those whereby they\n\
\ describe the detestable qualities of their _Yahoos_, among which they\n\
\ were not able to distinguish this of pride, for want of thoroughly\n\
\ understanding human nature, as it shows itself in other countries where\n\
\ that animal presides.  But I, who had more experience, could plainly\n\
\ observe some rudiments of it among the wild _Yahoos_.\n\
\ \n\
\ But the _Houyhnhnms_, who live under the government of reason, are no\n\
\ more proud of the good qualities they possess, than I should be for not\n\
\ wanting a leg or an arm; which no man in his wits would boast of,\n\
\ although he must be miserable without them.  I dwell the longer upon this\n\
\ subject from the desire I have to make the society of an English _Yahoo_\n\
\ by any means not insupportable; and therefore I here entreat those who\n\
\ have any tincture of this absurd vice, that they will not presume to come\n\
\ in my sight."
