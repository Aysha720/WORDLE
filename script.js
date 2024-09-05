const dataset = [ "aback", "abaft", "abase", "abate", "abbey", "abbot", "abhor", "abide", "abler", "abode", "about", "above", 
    "abuse", "abyss", "ached", "aches", "acids", "acorn", "acres", "acrid", "acted", "actor", "acute", "adage", 
    "adapt", "added", "adder", "adept", "adieu", "admit", "adobe", "adopt", "adore", "adorn", "adult", "aegis", 
    "aeons", "affix", "afire", "afoot", "after", "again", "agape", "agate", "agent", "agile", "aging", "aglow", 
    "agony", "agree", "ahead", "aided", "aides", "ailed", "aimed", "aired", "aisle", "alarm", "album", "alder", 
    "alert", "alias", "alibi", "alien", "alike", "alive", "allay", "alley", "allot", "allow", "alloy", "aloes", 
    "aloft", "alone", "along", "aloof", "aloud", "alpha", "altar", "alter", "altos", "amass", "amaze", "amber", 
    "amble", "amend", "amigo", "amiss", "amity", "among", "amour", "ample", "amply", "amuse", "angel", "anger", 
    "angle", "angry", "angst", "anime", "ankle", "annex", "annoy", "annul", "antes", "antic", "anvil", "apace", 
    "apart", "aping", "appal", "apple", "apply", "apron", "aptly", "areas", "arena", "argue", "arise", "armed", 
    "aroma", "arose", "array", "arrow", "arson", "ashen", "ashes", "aside", "asked", "askew", "aspen", "assay", 
    "asses", "asset", "aster", "astir", "atlas", "atoll", "atoms", "atone", "attar", "attic", "audio", "audit", 
    "auger", "aught", "augur", "aunts", "auras", "autos", "avail", "avers", "avert", "avoid", "avows", "await", 
    "awake", "award", "aware", "awful", "awoke", "axiom", "axles", "azure", "babel", "babes", "backs", "bacon", 
    "badge", "badly", "baggy", "baits", "baize", "baked", "baker", "bales", "balls", "balmy", "banal", "bands", 
    "bandy", "bangs", "banjo", "banks", "banns", "barbs", "bards", "bared", "barge", "barks", "barns", "baron", 
    "basal", "based", "baser", "bases", "basic", "basil", "basin", "basis", "basso", "baste", "batch", "bated", 
    "bathe", "baths", "baton", "bayou", "beach", "beads", "beady", "beaks", "beams", "beans", "beard", "bears", 
    "beast", "beaux", "beech", "beets", "befit", "began", "begat", "beget", "begin", "begot", "begun", "being", 
    "belie", "belle", "bells", "belly", "below", "belts", "bench", "bends", "bergs", "berry", "berth", "beryl", 
    "beset", "besom", "bevel", "bible", "bided", "bides", "bight", "bigot", "bilge", "bills", "billy", "binds", 
    "biped", "birch", "birds", "birth", "bison", "bitch", "bites", "black", "blade", "blame", "bland", "blank", 
    "blare", "blast", "blaze", "bleak", "bleat", "bleed", "blend", "blent", "bless", "blest", "blind", "blink", 
    "bliss", "block", "blocs", "blond", "blood", "bloom", "blots", "blown", "blows", "bluer", "blues", "bluff", 
    "blunt", "blurt", "blush", "board", "boars", "boast", "boats", "boded", "bodes", "boggy", "bogus", "boils", 
    "boles", "bolts", "bombs", "bonds", "boned", "bones", "bonny", "bonus", "booby", "books", "booms", "boons", 
    "boors", "boost", "booth", "boots", "booty", "booze", "borax", "bored", "bores", "borne", "bosom", "bough", 
    "bound", "bouts", "bowed", "bowel", "bower", "bowls", "boxed", "boxer", "boxes", "brace", "brags", "braid", 
    "brain", "brake", "brand", "brass", "brats", "brave", "bravo", "brawl", "brawn", "bread", "break", "breed", 
    "briar", "bribe", "brick", "bride", "brief", "brier", "brigs", "brims", "brine", "bring", "brink", "briny", 
    "brisk", "broad", "broil", "broke", "brood", "brook", "broom", "broth", "brown", "brows", "bruin", "brunt", 
    "brush", "brute", "bucks", "budge", "buggy", "bugle", "build", "built", "bulbs", "bulge", "bulks", "bulky", 
    "bulls", "bully", "bumps", "bunch", "bunks", "buoys", "burly", "burns", "burnt", "burro", "burrs", "burst", 
    "bushy", "busts", "butte", "butts", "buxom", "buyer", "cabal", "cabby", "cabin", "cable", "cacao", "cache", 
    "cadet", "cadre", "caged", "cages", "cairn", "caked", "cakes", "calls", "calms", "calyx", "camel", "cameo", 
    "camps", "canal", "candy", "canes", "canny", "canoe", "canon", "canto", "caper", "capes", "capon", "cards", 
    "cared", "cares", "cargo", "carol", "carry", "carts", "carve", "cased", "cases", "casks", "caste", "casts", 
    "catch", "cater", "cause", "caved", "caves", "cavil", "cease", "cedar", "ceded", "cells", "cents", "chafe", 
    "chaff", "chain", "chair", "chalk", "champ", "chant", "chaos", "chaps", "charm", "chart", "chary", "chase", 
    "chasm", "chats", "cheap", "cheat", "check", "cheek", "cheer", "chefs", "chess", "chest", "chick", "chide", 
    "chief", "child", "chill", "chime", "china", "chink", "chins", "chips", "chirp", "choir", "choke", "chops", 
    "chord", "chose", "chuck", "chump", "chums", "chunk", "churl", "churn", "chute", "cider", "cigar", "cinch", 
    "circa", "cited", "cites", "civet", "civic", "civil", "clack", "claim", "clamp", "clams", "clang", "clank", 
    "clans", "claps", "clash", "clasp", "class", "claws", "clean", "clear", "clefs", "cleft", "clerk", "click", 
    "cliff", "climb", "cling", "clink", "clips", "cloak", "clock", "clods", "clogs", "clone", "close", "cloth", 
    "cloud", "clout", "clove", "clown", "cloys", "clubs", "cluck", "clued", "clues", "clump", "clung", "clunk", 
    "coach", "coals", "coast", "coats", "cobra", "cocky", "cocoa", "coded", "codes", "coeds", "coils", "coins", 
    "colic", "colts", "combs", "comet", "comes", "comet", "comfy", "comic", "comma", "conch", "cones", "coney", 
    "conks", "cooed", "cooks", "cooky", "cools", "coops", "coots", "coral", "cords", "cored", "cores", "corns", 
    "corny", "corps", "costs", "couch", "cough", "could", "count", "coupe", "coups", "court", "cover", "covet", 
    "cowed", "cower", "crabs", "crack", "craft", "crags", "cramp", "crane", "crank", "crash", "crass", "crate", 
    "crave", "crawl", "craze", "crazy", "creak", "cream", "credo", "creed", "creek", "creep", "crepe", "crept", 
    "crest", "crews", "crick", "cried", "cries", "crime", "crimp", "crisp", "croak", "crook", "cross", "croup", 
    "crowd", "crown", "crows", "crude", "cruel", "crumb", "crush", "crust", "crypt", "cubed", "cubes", "cubic", 
    "cuffs", "culls", "cults", "curds", "cured", "cures", "curio", "curls", "curly", "curry", "curse", "curve", 
    "curvy", "cusps", "cuter", "cycle", "cynic", "daddy", "daffy", "daily", "dairy", "daisy", "dance", "dandy", 
    "dared", "dares", "darns", "dated", "dater", "dates", "datum", "daunt", "dawns", "deals", "dealt", "deans", 
    "dears", "deary", "death", "debit", "debts", "debug", "debut", "decal", "decay", "decks", "decor", "deeds", 
    "deems", "deeps", "deers", "defer", "deify", "deign", "deism", "deity", "delay", "delis", "dells", "delta", 
    "delve", "demon", "demur", "denim", "dense", "dents", "depth", "derby", "desks", "deter", "detox", "deuce", 
    "devil", "diary", "diced", "dices", "dicey", "diets", "digit", "dikes", "dined", "diner", "dines", "dingy", 
    "dinky", "diode", "dirge", "dirty", "disco", "ditch", "divan", "diver", "dives", "divot", "dizzy", "docks", 
    "dodge", "dodgy", "dogma", "doing", "dolce", "doles", "dolls", "dolor", "dolts", "domed", "domes", "donor", 
    "doors", "dopey", "doubt", "dough", "douse", "doves", "downs", "dozen", "draft", "drags", "drain", "drake", 
    "drama", "drank", "drape", "drawl", "drawn", "draws", "dread", "dream", "dregs", "dress", "dried", "drier", 
    "dries", "drift", "drill", "drily", "drink", "drive", "droll", "drone", "drool", "droop", "drops", "drove", 
    "drown", "drugs", "drums", "drunk", "dryly", "ducal", "duchy", "ducks", "duels", "duets", "dulls", "dully", 
    "dummy", "dumps", "dunce", "dunes", "dunks", "duped", "dupes", "dusks", "dusts", "dusty", "duvet", "dwarf", 
    "dweeb", "dwell", "dwelt", "dying", "eager", "eagle", "earls", "early", "earns", "earth", "eased", "easel", 
    "eases", "eaten", "eater", "eaves", "ebbed", "ebony", "edema", "edged", "edges", "edict", "edify", "edits", 
    "eerie", "egged", "egret", "eight", "eject", "elate", "elbow", "elder", "elect", "elegy", "elide", "elite", 
    "elope", "elude", "elves", "email", "embed", "ember", "emend", "emery", "emirs", "emits", "empty", "enact", 
    "ended", "ender", "endow", "enema", "enemy", "enjoy", "ennui", "ensue", "enter", "entry", "envoy", "epics", 
    "epoch", "epoxy", "equal", "equip", "erase", "erect", "erode", "erred", "error", "erupt", "essay", "ether", 
    "ethic", "ethos", "etude", "evade", "evens", "event", "every", "evict", "evils", "evoke", "exact", "exalt", 
    "exams", "excel", "exert", "exile", "exist", "exits", "expel", "extol", "extra", "exude", "exult", "eying", 
    "fable", "faced", "faces", "facet", "facts", "faded", "fades", "fails", "faint", "fairs", "fairy", "faith", 
    "faked", "fakes", "falls", "false", "famed", "fancy", "fangs", "farce", "fared", "fares", "farms", "farts", 
    "fatal", "fated", "fates", "fatty", "fault", "fauna", "favor", "fawns", "fazed", "fazes", "feast", "feats", 
    "feces", "feeds", "feels", "feign", "feint", "fella", "fells", "felon", "felts", "femme", "femur", "fence", 
    "fends", "feral", "ferns", "ferny", "ferry", "fetal", "fetch", "fetid", "fetus", "feuds", "fever", "fiats", 
    "fiber", "fibre", "ficus", "field", "fiend", "fiery", "fifth", "fifty", "fight", "filed", "files", "filet", 
    "fills", "filly", "films", "filth", "final", "finch", "finds", "fined", "finer", "fines", "finks", "fiord", 
    "fired", "fires", "firms", "first", "fishy", "fists", "fitly", "fives", "fixed", "fixer", "fixes", "fizzy", 
    "flack", "flags", "flail", "flair", "flake", "flaky", "flame", "flank", "flare", "flash", "flask", "flats", 
    "flaws", "flays", "fleas", "fleck", "fled", "fleet", "flesh", "flick", "flied", "flies", "fling", "flint", 
    "flirt", "float", "flock", "flood", "floor", "floss", "flour", "flout", "flown", "flows", "fluff", "fluid", 
    "fluke", "flume", "flung", "flush", "flute", "flyer", "foals", "foams", "foamy", "focal", "focus", "foggy", 
    "foist", "folds", "folio", "folks", "folly", "fonts", "foods", "fools", "foot","fop", "force", "fords", "forge", "forgo", "forks", "forms", "forte", "forth", "forts", "forty", "forum", "fouls", 
    "found", "fours", "fowls", "foxed", "foxes", "foyer", "frail", "frame", "franc", "frank", "frats", "fraud", 
    "freak", "freed", "freer", "frees", "fresh", "frets", "friar", "fried", "frier", "fries", "frill", "frisk", 
    "frock", "frogs", "front", "froth", "frown", "froze", "fruit", "frump", "fryer", "fucks", "fudge", "fuels", 
    "fugal", "fugit", "fugue", "fully", "fumed", "fumes", "funds", "fungi", "funks", "funky", "funny", "furls", 
    "furor", "furry", "fused", "fuses", "fussy", "fusty", "futon", "fuzed", "fuzes", "fuzzy", "gabby", "gable", 
    "gaffe", "gaged", "gages", "gaily", "gains", "gaits", "galas", "gales", "galls", "gamed", "gamer", "games", 
    "gamey", "gamin", "gamma", "gamut", "gangs", "gaols", "gaped", "gapes", "gases", "gasps", "gassy", "gated", 
    "gates", "gator", "gaudy", "gauge", "gaunt", "gauze", "gavel", "gawks", "gawky", "gayer", "gayly", "gazed", 
    "gazer", "gazes", "gears", "gecko", "geeks", "geeky", "geese", "gelds", "genes", "genie", "genii", "genre", 
    "gents", "genus", "geode", "germs", "germy", "getup", "ghost", "ghoul", "giant", "giddy", "gifts", "gilds", 
    "gills", "gimme", "gimpy", "ginny", "gipsy", "girls", "girly", "girth", "given", "giver", "gives", "glade", 
    "gland", "glare", "glass", "glaze", "gleam", "glean", "glees", "glens", "glide", "glint", "glitz", "gloat", 
    "globe", "globs", "gloom", "glory", "gloss", "glove", "glows", "glued", "glues", "gluey", "glums", "glyph", 
    "gnarl", "gnash", "gnats", "gnaws", "gnome", "goads", "goals", "goats", "godly", "going", "golds", "golfs", 
    "golly", "gonad", "goner", "gongs", "goods", "goody", "gooey", "goofs", "goofy", "goons", "goopy", "goose", 
    "gored", "gores", "gorge", "gouge", "gourd", "grabs", "grace", "grade", "grads", "graft", "grail", "grain", 
    "grams", "grand", "grant", "grape", "graph", "grasp", "grass", "grate", "grave", "gravy", "grays", "graze", 
    "great", "greed", "green", "greet", "grids", "grief", "grill", "grime", "grimy", "grind", "grins", "gripe", 
    "grips", "grist", "grits", "groan", "groin", "grope", "gross", "group", "grout", "grove", "growl", "grown", 
    "grows", "grubs", "gruel", "gruff", "grunt", "guard", "guava", "guess", "guest", "guide", "guild", "guile", 
    "guilt", "guise", "gulch", "gulfs", "gulls", "gully", "gulps", "gummy", "gunny", "guppy", "gusto", "gusts", 
    "gusty", "gutsy", "guyed", "gyros", "habit", "hacks", "hairs", "hairy", "hallo", "halls", "halos", "halts", 
    "hands", "handy", "hangs", "hanky", "happy", "hardy", "harem", "hares", "harks", "harms", "harps", "harry", 
    "harsh", "haste", "hasty", "hatch", "hated", "hater", "hates", "hauls", "haunt", "haute", "haven", "haves", 
    "havoc", "hawks", "hazel", "hazed", "hazes", "heads", "heady", "heals", "heaps", "heard", "hears", "heart", 
    "heath", "heats", "heave", "heavy", "hedge", "heeds", "heels", "hefts", "hefty", "heist", "helix", "hello", 
    "hells", "helms", "helps", "hence", "herbs", "herds", "heron", "hertz", "hewed", "hicks", "hider", "hides", 
    "highs", "hiked", "hiker", "hikes", "hills", "hints", "hired", "hires", "hitch", "hives", "hoagy", "hoard", 
    "hoars", "hobby", "hobos", "hocks", "hoist", "holds", "holed", "holes", "holly", "homed", "homer", "homes", 
    "homey", "honed", "hones", "honey", "honks", "honor", "hooch", "hoods", "hooey", "hoofs", "hooks", "hoops", 
    "hoots", "hoped", "hopes", "hoppy", "horde", "horns", "horny", "horse", "hosed", "hoses", "hosts", "hotel", 
    "hound", "hours", "house", "hovel", "hover", "howdy", "howls", "hubby", "huffs", "huffy", "huggy", "hulks", 
    "hulky", "hullo", "hulls", "human", "humid", "humor", "humps", "hunch", "hunks", "hunky", "hunts", "hurls", 
    "hurly", "hurts", "husks", "husky", "hussy", "hutch", "hydra", "hydro", "hyena", "hying", "hyped", "hyper", 
    "hypes", "hymns", "hyped", "icons", "ideal", "ideas", "idiom", "idiot", "idler", "idles", "idols", "igloo", 
    "ikons", "iliac", "image", "imbed", "imbue", "impel", "imply", "inane", "inapt", "inbox", "incur", "index", 
    "inept", "inert", "infer", "infra", "ingot", "inked", "inlay", "inlet", "inner", "input", "inset", "inter", 
    "intro", "iodic", "ionic", "irate", "irons", "irony", "isles", "itchy", "items", "ivied", "ivies", "ivory", 
    "jacks", "jaded", "jades", "jails", "jambs", "japan", "japed", "japes", "jars", "jazzy", "jeans", "jelly", 
    "jerks", "jerky", "jests", "jetty", "jiffy", "jihad", "jilts", "jived", "jiver", "jives", "joins", "joint", 
    "joist", "joked", "joker", "jokes", "jolly", "jolts", "jowls", "judge", "juice", "juicy", "jumps", "jumpy", 
    "junks", "junky", "juror", "justs", "jutes", "kabob", "kales", "karts", "kebab", "keels", "keeps", "keg", 
    "kelps", "kicks", "kinky", "kites", "kitty", "knack", "knead", "kneel", "knees", "knelt", "knife", "knits", 
    "knobs", "knock", "knots", "known", "knows", "koala", "kooky", "kraut", "krill", "label", "labor", "laced", 
    "laces", "lacks", "laded", "lades", "lager", "laird", "lakes", "lamed", "lamer", "lames", "lamps", "lance", 
    "lands", "lanes", "lanky", "lapel", "lapse", "larch", "lards", "large", "larks", "larva", "lasso", "lasts", 
    "latch", "later", "latex", "laths", "latte", "laugh", "lawns", "laxer", "laxly", "layer", "layup", "leach", 
    "leads", "leafy", "leaks", "leans", "leant", "leaps", "learn", "lease", "leash", "least", "leave", "ledge", 
    "leech", "leery", "lefts", "lefty", "legal", "leggy", "legit", "lemon", "lemur", "lends", "lento", "leper", 
    "levee", "level", "lever", "liars", "libel", "licks", "lifts", "light", "liked", "liken", "liker", "likes", 
    "lilac", "lilts", "limbo", "limbs", "limed", "limes", "limit", "limps", "lined", "linen", "liner", "lines", 
    "lingo", "links", "lints", "lions", "lipid", "lisps", "lists", "liter", "lithe", "lived", "liven", "liver", 
    "lives", "livid", "llama", "loafs", "loamy", "loans", "loath", "lobby", "lobed", "lobes", "local", "locks", 
    "locus", "lodes", "lodge", "lofts", "lofty", "logic", "login", "logos", "loins", "lolls", "lolly", "loner", 
    "longs", "looks", "looms", "loons", "loony", "loops", "loose", "loots", "loped", "lopes", "lords", "lorry", 
    "loser", "loses", "lotto", "lotus", "louse", "lousy", "loved", "lover", "loves", "lower", "lowly", "loxes", 
    "lucky", "lumen", "lumps", "lunar", "lunch", "lunge", "lungs", "lupus", "lurch", "lured", "lures", "lurid", 
    "lusts", "lusty", "lutes", "lying", "lymph", "lynch", "lyric", "macaw", "macho", "macro", "madam", "madly", 
    "mafia", "magic", "magma", "magna", "maids", "mails", "maims", "mains", "maize", "major", "maker", "makes", 
    "males", "malts", "mamas", "mambo", "mamma", "mammy", "maned", "manes", "mango", "mangy", "mania", "manic", 
    "manly", "manna", "manor", "maple", "march", "marks", "marry", "marsh", "marts", "masks", "mason", "masse", 
    "match", "mated", "mater", "mates", "maths", "matte", "matzo", "mauls", "mauve", "maxim", "maybe", "mayor", 
    "meals", "means", "meant", "meats", "meaty", "mecca", "medal", "media", "medic", "meets", "melee", "melon", 
    "melts", "memes", "memos", "mends", "menus", "meows", "mercy", "merge", "merit", "merry", "messy", "metal", 
    "meted", "meter", "metes", "meths", "metro", "mewed", "micro", "midge", "midst", "might", "miked", "mikes", 
    "milks", "milky", "mills", "mimed", "mimes", "mimic", "mince", "minds", "mined", "miner", "mines", "minim", 
    "minis", "minks", "minor", "mints", "minty", "minus", "mired", "mires", "mirth", "miser", "missy", "mists", 
    "misty", "miter", "mixes", "moans", "moats", "mocha", "mocks", "modal", "model", "modem", "modes", "mogul", 
    "moist", "molar", "molds", "moldy", "moles", "molts", "momma", "mommy", "monks", "moody", "moods", "moons", 
    "moors", "moose", "moots", "moped", "mopes", "moral", "moray", "morel", "mores", "morns", "moron", "morph", 
    "mossy", "motel", "motes", "moths", "mothy", "motif", "motor", "motto", "mould", "moult", "mound", "mount", 
    "mourn", "mouse", "mousy", "mouth", "moved", "mover", "moves", "movie", "mowed", "mower", "mucky", "mucus", 
    "muddy", "muffs", "mulch", "mule", "multi", "mumps", "munch", "muons", "mural", "murder", "murky", "mused", 
    "muses", "mushy", "music", "musky", "musts", "musty", "muted", "muter", "mutes", "mutts", "myths", "nacho", 
    "nadir", "naive", "naked", "named", "names", "nanny", "nards", "nasal", "nasty", "natal", "naval", "navel", 
    "nears", "neath", "necks", "needs", "needy", "neigh", "nerdy", "nerds", "nerve", "nests", "never", "newer", 
    "newly", "nexus", "nicer", "niche", "niece", "night", "nines", "ninja", "ninny", "ninth", "nippy", "noble", 
    "nobly", "nodal", "nodes", "noisy", "noise", "nomad", "nonce", "nooks", "noons", "noose", "north", "nosed", 
    "noses", "notch", "noted", "notes", "novel", "noway", "nudge", "nukes", "numbs", "nurse", "nutty", "nylon", 
    "nymph", "oaken", "oakum", "oared", "oases", "oasis", "oaths", "obese", "obeys", "obits", "oboes", "occur", 
    "ocean", "ochre", "octal", "octet", "odder", "oddly", "odium", "odors", "offed", "offer", "often", "ogled", 
    "ogles", "oiled", "okays", "olden", "older", "olive", "ombre", "omega", "omens", "onion", "onset", "oozed", 
    "oozes", "opals", "opens", "opera", "opine", "optic", "orbit", "order", "organ", "osier", "other", "otter", 
    "ought", "ounce", "outdo", "outer", "outgo", "ovals", "ovary", "ovens", "overs", "overt", "owing", "owned", 
    "owner", "oxbow", "oxide", "ozone", "paced", "paces", "packs", "pacts", "paddy", "padre", "pagan", "paged", 
    "pager", "pages", "pails", "pains", "paint", "pairs", "paled", "paler", "pales", "palls", "palsy", "palms", 
    "panel", "panes", "pangs", "panic", "pansy", "pants", "papal", "paper", "pappy", "paras", "parch", "pared", 
    "parer", "pares", "parka", "parks", "parry", "parse", "parts", "party", "pasta", "paste", "pasts", "pasty", 
    "patch", "patio", "patsy", "patty", "pause", "paved", "paves", "pawed", "pawns", "payer", "peace", "peach", 
    "peaks", "pearl", "pears", "pecan", "pecks", "pedal", "peeks", "peels", "peeps", "peers", "penal", "pence", 
    "penis", "penne", "penny", "perch", "peril", "perks", "perky", "perms", "perry", "pesky", "pesto", "pests", 
    "petal", "petty", "piano", "picks", "piece", "piers", "piety", "piggy", "piked", "piker", "pikes", "pilaf", 
    "piled", "piles", "pills", "pilot", "pinch", "pined", "pines", "pings", "pinky", "pinup", "pious", "piped", 
    "piper", "pipes", "pique", "pitas", "pitch", "pithy", "pitta", "pivot", "pixel", "pixie", "pizza", "place", 
    "plaid", "plain", "plait", "plane", "plank", "plans", "plant", "plate", "plaza", "plead", "pleas", "pleat", 
    "plied", "plies", "plods", "plops", "plots", "plows", "ploys", "pluck", "plugs", "plumb", "plume", "plump", 
    "plums", "plush", "poach", "poems", "poets", "point", "poise", "poked", "poker", "pokes", "polar", "poles", 
    "polls", "polyp", "ponds", "pools", "poops", "popes", "poppy", "porch", "pored", "pores", "porky", "ports", 
    "posed", "poser", "poses", "posit", "posse", "posts", "pouch", "poufs", "pound", "pours", "pouts", "power", 
    "poxes", "prank", "prats", "prawn", "prays", "preen", "preps", "press", "preys", "price", "prick", "pride", 
    "pried", "pries", "prime", "primo", "print", "prior", "prism", "privy", "prize", "probe", "promo", "proms", 
    "prone", "prong", "proof", "props", "prose", "proud", "prove", "prowl", "proxy", "prude", "prune", "psalm", 
    "pseud", "puck","pucks", "puffy", "puked", "pukes", "pulls", "pulps", "pulse", "pumas", "pumps", "punch", "punks", "punts", 
    "pupae", "pupal", "pupil", "puppy", "puree", "purer", "purge", "purls", "purrs", "purse", "pushy", "putts", 
    "quack", "quads", "quaff", "quail", "quake", "qualm", "quark", "quart", "quash", "quasi", "queen", "queer", 
    "quell", "query", "quest", "queue", "quick", "quiet", "quill", "quilt", "quint", "quips", "quirk", "quite", 
    "quits", "quota", "quote", "quoth", "rabbi", "rabid", "raced", "racer", "races", "racks", "radar", "radii", 
    "radio", "radix", "radon", "rafts", "raged", "rages", "raids", "rails", "rains", "rainy", "raise", "raked", 
    "rakes", "rally", "ramps", "ranch", "randy", "range", "ranks", "rants", "raped", "raper", "rapes", "rapid", 
    "rarer", "rasps", "rated", "rates", "ratio", "ratty", "raved", "ravel", "raven", "raves", "rayon", "razed", 
    "razes", "razor", "reach", "react", "reads", "ready", "realm", "reaps", "rears", "rebel", "rebid", "rebus", 
    "rebut", "recap", "recon", "recto", "recur", "recut", "reeds", "reedy", "refer", "refit", "refly", "regal", 
    "rehab", "reign", "reins", "relax", "relay", "relic", "reman", "remit", "remix", "renal", "rends", "renew", 
    "rents", "repay", "repel", "reply", "repos", "reset", "resin", "rests", "retro", "retry", "reuse", "revel", 
    "revue", "rhino", "rhyme", "rider", "rides", "ridge", "rifer", "riffs", "rifle", "rifts", "right", "rigid", 
    "rigor", "riled", "riles", "rinds", "rings", "rinks", "rinse", "riots", "ripen", "riper", "risen", "riser", 
    "rises", "risks", "risky", "rites", "rival", "riven", "river", "rives", "roads", "roams", "roars", "roast", 
    "robed", "robes", "robin", "robot", "rocks", "rocky", "rodeo", "roger", "rogue", "roles", "rolls", "roman", 
    "romps", "roofs", "rooms", "roomy", "roost", "roots", "roped", "ropes", "roses", "rotor", "rouge", "rough", 
    "round", "rouse", "route", "routs", "roved", "rover", "roves", "rowdy", "rowed", "royal", "rubes", "ruble", 
    "ruche", "ruddy", "ruder", "ruffs", "rugby", "ruins", "ruled", "ruler", "rules", "rumba", "rummy", "rumor", 
    "rupee", "rural", "ruses", "rusts", "rusty", "sadly", "safes", "sager", "sages", "sails", "saint", "sakes", 
    "salad", "sales", "salon", "salsa", "salts", "salty", "salve", "salvo", "sands", "sandy", "saner", "sappy", 
    "sassy", "satin", "satyr", "sauce", "saucy", "sauna", "saved", "saver", "saves", "savor", "savvy", "sawed", 
    "sawer", "saxes", "scabs", "scald", "scale", "scalp", "scams", "scans", "scant", "scare", "scarf", "scary", 
    "scene", "scent", "scoff", "scold", "scoop", "scoot", "scope", "score", "scorn", "scour", "scout", "scowl", 
    "scram", "scrap", "screw", "scrub", "scrum", "scuba", "scuff", "seals", "seams", "sears", "seats", "sects", 
    "sedan", "seeds", "seedy", "seeks", "seems", "seeps", "seers", "segue", "seine", "seize", "sells", "semen", 
    "semi", "senor", "sense", "sepal", "sepia", "serfs", "serif", "serum", "serve", "setup", "seven", "sever", 
    "sewed", "sewer", "sexed", "sexes", "shack", "shade", "shady", "shaft", "shags", "shake", "shaky", "shale", 
    "shall", "shalt", "shame", "shams", "shank", "shape", "shard", "share", "shark", "sharp", "shave", "shawl", 
    "shear", "sheds", "sheen", "sheep", "sheer", "sheet", "shelf", "shell", "shift", "shine", "shiny", "ships", 
    "shire", "shirk", "shirt", "shoes", "shone", "shook", "shoot", "shops", "shore", "short", "shots", "shout", 
    "shove", "shown", "shows", "showy", "shrub", "shrug", "shuns", "shush", "shuts", "sided", "sides", "siege", 
    "sieve", "sight", "signs", "silks", "silky", "sills", "silly", "since", "sinew", "singe", "sings", "sinks", 
    "sinus", "sired", "siren", "sires", "sites", "sixes", "sixth", "sixty", "sized", "sizer", "sizes", "skate", 
    "skews", "skids", "skied", "skier", "skies", "skiff", "skill", "skimp", "skins", "skirt", "skits", "skulk", 
    "skull", "skunk", "slabs", "slack", "slain", "slake", "slams", "slang", "slant", "slaps", "slash", "slate", 
    "slats", "slave", "slaws", "sleek", "sleep", "sleet", "slept", "slice", "slick", "slide", "slime", "slimy", 
    "sling", "slink", "slips", "slits", "slobs", "slogs", "sloop", "slope", "slosh", "sloth", "slots", "slows", 
    "slued", "slues", "slump", "slung", "slurp", "slush", "smack", "small", "smart", "smash", "smear", "smell", 
    "smelt", "smile", "smirk", "smite", "smith", "smock", "smoke", "smoky", "smote", "snack", "snags", "snail", 
    "snake", "snaps", "snare", "snarl", "sneak", "sneer", "snide", "snipe", "snobs", "snoop", "snoot", "snore", 
    "snort", "snout", "snows", "snowy", "snubs", "snuff", "soaks", "soaps", "soapy", "soars", "sober", "socks", 
    "sodas", "sofas", "softy", "soggy", "soils", "solar", "soled", "soles", "solid", "solve", "sonar", "songs", 
    "sonic", "sooth", "sooty", "sorry", "sorts", "souls", "sound", "soups", "soupy", "south", "sowed", "sower", 
    "space", "spade", "spank", "spans", "spare", "spark", "spasm", "spate", "spawn", "spays", "speak", "spear", 
    "speck", "specs", "speed", "spell", "spend", "spent", "sperm", "spews", "spice", "spicy", "spied", "spiel", 
    "spies", "spike", "spiky", "spill", "spilt", "spine", "spins", "spiny", "spire", "spite", "spits", "spitz", 
    "splat", "splay", "split", "spoil", "spoke", "spoof", "spook", "spool", "spoon", "spore", "sport", "spots", 
    "spout", "spray", "spree", "sprig", "spuds", "spunk", "spurn", "spurs", "spurt", "squad", "squat", "squaw", 
    "squib", "stack", "staff", "stage", "stags", "stain", "stair", "stake", "stale", "stalk", "stall", "stamp", 
    "stand", "stank", "stare", "stark", "stars", "start", "stash", "state", "stats", "stave", "stays", "stead", 
    "steak", "steal", "steam", "steed", "steel", "steep", "steer", "stems", "steps", "stews", "stick", "stiff", 
    "stile", "still", "stilt", "sting", "stink", "stint", "stirs", "stock", "stoic", "stoke", "stole", "stomp", 
    "stone", "stony", "stood", "stool", "stoop", "stops", "store", "stork", "storm", "story", "stout", "stove", 
    "strap", "straw", "stray", "strip", "strop", "strum", "strut", "stubs", "stuck", "studs", "study", "stuff", 
    "stump", "stung", "stunk", "stunt", "style", "suave", "sucks", "sucre", "sudan", "sudsy", "suede", "sugar", 
    "sulks", "sulky", "sully", "sumac", "sumps", "sunny", "super", "supra", "surfs", "surge", "surly", "sushi", 
    "sutra", "swabs", "swags", "swamp", "swank", "swans", "swaps", "sward", "swarm", "swash", "swath", "swats", 
    "sways", "swear", "sweat", "sweep", "sweet", "swell", "swept", "swift", "swigs", "swill", "swims", "swine", 
    "swing", "swipe", "swirl", "swish", "swiss", "swoon", "swoop", "sword", "swore", "sworn", "swung", "synch", 
    "syncs", "synod", "syrup", "tabby", "table", "taboo", "tacit", "tacks", "tacky", "tacos", "tails", "tains", 
    "taint", "taken", "taker", "takes", "tales", "talks", "talky", "tally", "talon", "tamed", "tamer", "tames", 
    "tango", "tangs", "tangy", "tanks", "taped", "taper", "tapes", "tapir", "tardy", "tared", "tares", "tarns", 
    "tarts", "tasks", "taste", "tasty", "taxed", "taxer", "taxes", "teach", "teaks", "teams", "tears", "teary", 
    "tease", "teens", "teeny", "teeth", "tells", "tempo", "temps", "tempt", "tends", "tenet", "tenor", "tense", 
    "tenth", "tents", "tepee", "tepid", "terms", "terns", "tests", "testy", "texts", "thank", "thaws", "theft", 
    "their", "theme", "thens", "there", "these", "thick", "thief", "thigh", "thine", "thing", "think", "thins", 
    "third", "thong", "thorn", "those", "three", "threw", "throb", "throw", "thrum", "thuds", "thugs", "thumb", 
    "thump", "thus", "thyme", "tiara", "tibia", "ticks", "tidal", "tides", "tiers", "tiffs", "tiger", "tight", 
    "tilde", "tiled", "tiler", "tiles", "tills", "tilts", "timed", "timer", "times", "timid", "tines", "tinge", 
    "tired", "tires", "titan", "tithe", "title", "tizzy", "toads", "toast", "today", "toddy", "toffs", "toils", 
    "token", "tolls", "tombs", "tomes", "tonal", "toned", "toner", "tones", "tongs", "tonic", "tools", "toons", 
    "tooth", "topaz", "toped", "topee", "toper", "topes", "topic", "topos", "torch", "torso", "torte", "torts", 
    "total", "totem", "totes", "touch", "tough", "tours", "touts", "towed", "towel", "tower", "towns", "toxic", 
    "toxin", "toys", "trace", "track", "tract", "trade", "trail", "train", "trait", "tramp", "trams", "traps", 
    "trash", "trawl", "trays", "tread", "treat", "trees", "treks", "trend", "tress", "triad", "trial", "tribe", 
    "trice", "trick", "tried", "trier", "tries", "trims", "trios", "tripe", "trips", "troll", "troop", "trope", 
    "trots", "trout", "trove", "truce", "truck", "truer", "truly", "trump", "trunk", "truss", "trust", "truth", 
    "tryst", "tubas", "tubby", "tuber", "tubes", "tucks", "tufts", "tulip", "tummy", "tunas", "tuned", "tuner", 
    "tunes", "tunic", "turbo", "turns", "tusks", "tutor", "twain", "twang", "tweak", "tweed", "tweet", "twice", 
    "twigs", "twins", "twirl", "twist", "twixt", "tying", "typed", "types", "ulcer", "ultra", "umber", "umbra", 
    "unapt", "unarm", "unary", "unbar", "unbox", "uncap", "uncle", "uncut", "under", "undid", "undue", "unfed", 
    "unfit", "unfix", "unify", "union", "unite", "units", "unity", "unlit", "unmet", "unset", "untie", "until", 
    "unwed", "unzip", "upped", "upper", "upset", "urban", "urged", "urges", "urine", "usage", "users", "usher", 
    "using", "usual", "usurp", "usury", "utter", "vague", "valet", "valid", "valor", "value", "valve", "vamps", 
    "vases", "vault", "veers", "vegan", "veils", "veins", "veiny", "velds", "vends", "venom", "vents", "venue", 
    "verbs", "verge", "verse", "vicar", "vices", "video", "views", "vigil", "vigor", "viler", "villa", "vines", 
    "vinyl", "viola", "viper", "viral", "vireo", "virus", "visas", "visit", "visor", "vista", "vivid", "vixen", 
    "vocal", "vodka", "vogue", "voice", "voids", "volts", "voted", "voter", "votes", "vouch", "vowed", "vowel", 
    "vulva", "wacko", "wacky", "waded", "wader", "wades", "wafer", "waged", "wager", "wages", "wagon", "wails", 
    "waist", "waits", "waive", "wakes", "wales", "walks", "walls", "waltz", "waned", "wanes", "wants", "wards", 
    "wares", "warms", "warns", "warps", "wasps", "waste", "watch", "water", "waved", "waver", "waves", "waxed", 
    "waxen", "waxes", "weary", "weave", "wedge", "weeds", "weedy", "weeks", "weeps", "weepy", "weigh", "weird", 
    "welds", "wells", "welsh", "welts", "wench", "whack", "whale", "whams", "wharf", "wheat", "wheel", "whelp", 
    "where", "which", "whiff", "while", "whims", "whine", "whiny", "whips", "whirl", "whisk", "white", "whizz", 
    "whole", "whoop", "whore", "whose", "wicks", "widen", "wider", "widow", "width", "wield", "wight", "wilds", 
    "wiled", "wiles", "wills", "willy", "wilts", "wimps", "wimpy", "winds", "windy", "wined", "wines", "wings", 
    "winks", "wiped", "wiper", "wipes", "wired", "wires", "wised", "wiser", "wises", "wisps", "wispy", "witch", 
    "wived", "wives", "woken", "wombs", "women", "wonky", "woods", "woody", "wooed", "wooer", "woofs", "wooly", 
    "woozy", "words", "wordy", "works", "world", "worms", "worry", "worse", "worst", "worth", "would", "wound", 
    "woven", "wraps", "wrath", "wreak", "wreck", "wrens", "wrest", "wring", "wrist", "write", "writs", "wrong", 
    "wrote", "wrung", "wryer", "wryly", "yacht", "yahoo", "yanks", "yards", "yarns", "yawed", "yawls", "yawns", 
    "yearn", "years", "yeast", "yells", "yelps", "yield", "yikes", "yodel", "yogas", "yogic", "yoked", "yokel", 
    "yokes", "yolks", "young", "yours", "youth", "yowls", "yucca", "yummy", "zebra", "zebus", "zeros", "zests", 
    "zesty", "zilch", "zincs", "zingy", "zippy", "zones", "zoned", "zooms"];

const buttonElements = document.querySelectorAll('button');
let row=1;
let letter=1;
const wordElements=document.querySelectorAll('.word-row');
const wordForTheDay = dataset[Math.floor(Math.random()*dataset.length)];
let gameOver=false;
let guessedCorrectly=false;

buttonElements.forEach((element) => {
    element.addEventListener('click', function(){
        keypress(element.attributes["data-key"].value)
    });
});

document.addEventListener('keydown', (event) => {
    const key = event.key;

    if ((key >= 'A' && key <= 'Z') || (key >= 'a' && key <= 'z') || key === 'Enter' || key === 'Delete') {
        keypress(key);
    }
});


function populateWord(key){
    if(letter<6){
        wordElements[row-1].querySelectorAll('.word')[letter-1].innerText=key.toUpperCase();
        letter+=1;
    }
    
}
  
function checkWord() {
    const letterElements = wordElements[row - 1].querySelectorAll('.word');
    let numOfCorrectLetters = 0;

    letterElements.forEach((element, index) => {
        const indexOfTheDay = wordForTheDay.toLocaleLowerCase().indexOf(element.innerText.toLowerCase());

        setTimeout(() => {
            element.classList.add('flip');  

            if (indexOfTheDay === index) {
                numOfCorrectLetters += 1;
                element.classList.add('word-green');
            } else if (indexOfTheDay > 0) {
                element.classList.add('word-yellow');
            } else {
                element.classList.add('word-grey');
            }
        }, index * 500);  
    });

    setTimeout(() => {
        if (numOfCorrectLetters === 5) {
            gameOver = true;
            guessedCorrectly = true;
            alert("Congratulations! You have guessed the word!");
        } else if (row > 6) {
            alert("Better luck next time! The word was: " + wordForTheDay);
        }
    }, letterElements.length * 500);  
}

function enterWord(){
    if(letter<6){
        alert("Not enough letters");
    }
    else{
        checkWord();
        row+=1;
        letter=1;
    }
}

function deleteLetter(){
    const letterElements = wordElements[row-1].querySelectorAll('.word');
    
    for(let i=letterElements.length-1;i>=0;i--){
        const element=letterElements[i];
            if(element.innerText !== ''){
            element.innerText='';
            letter-=1;
            break;
        }
    }
}


function keypress(key){
    if(!gameOver){
        if(key.toLowerCase()==='enter'){
            enterWord();
        }
        else if(key.toLowerCase()==='delete'){
            deleteLetter();
        }   
        else{
            populateWord(key);
        }
    }
    else{
        alert("Game Over! Try Again");
    }
}


const rulesPopup = document.getElementById("rules-popup");
const rulesBtn = document.getElementById("rules-btn");
const closeBtn = document.querySelector(".close-btn");

rulesBtn.addEventListener("click", function() {
    rulesPopup.style.display = "block";
});

closeBtn.addEventListener("click", function() {
    rulesPopup.style.display = "none";
});

window.addEventListener("click", function(event) {
    if (event.target == rulesPopup) {
        rulesPopup.style.display = "none";
    }
});
