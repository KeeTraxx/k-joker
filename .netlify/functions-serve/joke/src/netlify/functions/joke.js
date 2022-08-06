var __defProp = Object.defineProperty;
var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __getOwnPropSymbols = Object.getOwnPropertySymbols;
var __hasOwnProp = Object.prototype.hasOwnProperty;
var __propIsEnum = Object.prototype.propertyIsEnumerable;
var __defNormalProp = (obj, key, value) => key in obj ? __defProp(obj, key, { enumerable: true, configurable: true, writable: true, value }) : obj[key] = value;
var __spreadValues = (a, b) => {
  for (var prop in b || (b = {}))
    if (__hasOwnProp.call(b, prop))
      __defNormalProp(a, prop, b[prop]);
  if (__getOwnPropSymbols)
    for (var prop of __getOwnPropSymbols(b)) {
      if (__propIsEnum.call(b, prop))
        __defNormalProp(a, prop, b[prop]);
    }
  return a;
};
var __markAsModule = (target) => __defProp(target, "__esModule", { value: true });
var __export = (target, all) => {
  for (var name in all)
    __defProp(target, name, { get: all[name], enumerable: true });
};
var __reExport = (target, module2, copyDefault, desc) => {
  if (module2 && typeof module2 === "object" || typeof module2 === "function") {
    for (let key of __getOwnPropNames(module2))
      if (!__hasOwnProp.call(target, key) && (copyDefault || key !== "default"))
        __defProp(target, key, { get: () => module2[key], enumerable: !(desc = __getOwnPropDesc(module2, key)) || desc.enumerable });
  }
  return target;
};
var __toCommonJS = /* @__PURE__ */ ((cache) => {
  return (module2, temp) => {
    return cache && cache.get(module2) || (temp = __reExport(__markAsModule({}), module2, 1), cache && cache.set(module2, temp), temp);
  };
})(typeof WeakMap !== "undefined" ? /* @__PURE__ */ new WeakMap() : 0);

// netlify/functions/joke.ts
var joke_exports = {};
__export(joke_exports, {
  handler: () => handler
});

// netlify/functions/jokes/jokes-cs.json
var info = {
  formatVersion: 3
};
var jokes = [
  {
    category: "Misc",
    type: "single",
    joke: "Zn\xE1m spoustu vtip\u016F ve znakov\xE9 \u0159e\u010Di, kter\xE9 nikdo nesly\u0161el!",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 0,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "Uve\u010Fte holou v\u011Btu, kde bude jenom podm\u011Bt a p\u0159\xEDsudek.",
    delivery: "\u010Cech abstinuje.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 3,
    safe: true
  }
];
var jokes_cs_default = {
  info,
  jokes
};

// netlify/functions/jokes/jokes-de.json
var info2 = {
  formatVersion: 3
};
var jokes2 = [
  {
    category: "Pun",
    type: "twopart",
    setup: "Was bist du, wenn du nicht wei\xDFt, was du tun sollst, nachdem dein Fahrrad geklaut wurde?",
    delivery: "Radlos.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 0,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Wie nennt man einen schwulen, exotischen Vogel, der Kinder hat?",
    delivery: "Einen Papagay.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 1,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Wie hei\xDFt ein d\xFCnner Mensch mit Bulimie?",
    delivery: "Brechstange.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 2,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: 'Was lebt im Wald und schreit "Kugel"?',
    delivery: "Der Kugelschreib\xE4r.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 3,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Wie nennt sich ein Vogel mit Daddy-Fetisch?",
    delivery: "Ein Papageil.",
    flags: {
      nsfw: true,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: true
    },
    id: 4,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Im Bundestag gibt es neue Auflagen.",
    delivery: "Die St\xFChle waren zu unbequem.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 5,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Warum hat niemand das Getr\xE4nk des Soldaten gefunden?",
    delivery: "Es war in einer Camouflasche.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 6,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Eine notgeile, inkontinente Henne flattert zum Arzt. Die Diagnose?",
    delivery: "Hahndrang.",
    flags: {
      nsfw: true,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: true
    },
    id: 7,
    safe: false
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Kurzarbeit?",
    delivery: "Sind das dann 7 Bit?",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 8,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Wie schreibt ein Oktopus?",
    delivery: "Krakelig.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 9,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Warum ist der \xE4gyptische Totengott kein erfolgreicher Pro-Gamer?",
    delivery: "Weil er Anubis.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 10,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Wie nennt man ein ungerubbeltes Rubbellos?",
    delivery: "Rubbellos.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 11,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Ziemlich unfreundlich diese \xC4lpler.",
    delivery: '\xDCberall haben sie Schilder, auf denen steht "Wander weg"!',
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 12,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Was verwendet eine \xE4gyptische Pharaonin um ihren Hintern zu wischen?",
    delivery: "Kleopapier.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 13,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: 'Die Selbsthilfegruppe "HTML-Sonderzeichen-Probleme" trifft sich heute im gro&szlig;en Saal.',
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 14,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "Wenn man diese CD r\xFCckw\xE4rts spielt, sind satanische Verse zu h\xF6ren.\nViel schlimmer, wenn man sie vorw\xE4rts spielt, installiert sie Windows.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 15,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: 'Die Mutter schickt ihren Sohn mit folgender Einkaufsliste in den Supermarkt: "Eine Packung Milch, und wenn die Eier haben, bring drei Packungen mit."\nIm Supermarkt stellt der Sohn fest, es gibt dort Eier, also bringt er drei Packungen Milch mit.',
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 16,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: 'T\xE4glich verschwinden hunderte Senioren im Netz, weil sie "Alt" und "Entf" dr\xFCcken.',
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 17,
    safe: true
  },
  {
    category: "Misc",
    type: "single",
    joke: "Facebook ist wie ein Gef\xE4ngnis. Man sitzt rum, verschwendet Zeit, schreibt an W\xE4nde und wird angestupst von Leuten die man nicht kennt.",
    flags: {
      nsfw: true,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: true
    },
    id: 18,
    safe: false
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Es gibt 10 Arten von Menschen.",
    delivery: "Die einen verstehen das Bin\xE4re System, die anderen nicht.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 19,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Was macht ein Informatiker wenn sein Wagen nicht mehr anspringt?",
    delivery: "Aussteigen, einsteigen und nochmal starten.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 20,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: 'Treffen sich ein Informatiker und ein Wirtschaftsinformatiker.\nInformatiker: "Hast Du schon das neue Ubuntu?"\nDer Wirtschaftsinformatiker: "Nein, ich steh nicht auf Pokemon."',
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 21,
    safe: true
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "Was stellt sich ein Informatiker zu Weihnachten in die Wohnung?",
    delivery: "Einen B-Baum.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 22,
    safe: true
  },
  {
    category: "Misc",
    type: "single",
    joke: "Wer zuletzt lacht ... hat den h\xF6chsten Ping.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 23,
    safe: true
  },
  {
    category: "Misc",
    type: "single",
    joke: "Das echte Leben nervt ... aber die Grafik ist gut.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 24,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "Deine Mutter ist wie ein L3-Cache. Sie wird zwischen allen 4 Kernen durchgereicht und jeder hat Zugriff.",
    flags: {
      nsfw: true,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: true
    },
    id: 25,
    safe: false
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Was ist die Lieblingsbesch\xE4ftigung von Bits?",
    delivery: "Busfahren.",
    flags: {
      nsfw: false,
      racist: false,
      sexist: false,
      religious: false,
      political: false,
      explicit: false
    },
    id: 26,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "Treffen sich zwei Kerzen.",
    delivery: 'Fragt die eine "Was machst du heute?"\nSagt die andere "Ich gehe aus."',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 27,
    safe: true
  },
  {
    category: "Misc",
    type: "single",
    joke: "Ich hab gerade den DJ angerufen. Er hat aufgelegt.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 28,
    safe: true
  },
  {
    category: "Misc",
    type: "single",
    joke: 'Ein Beamter zum anderen: "Was haben die Leute nur, wir tun doch nichts!"',
    flags: {
      nsfw: false,
      religious: false,
      political: true,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 29,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Warst du eigentlich schonmal in Las Vegas?",
    delivery: "Nevada noch nie.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 30,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Wenn dir von Baguette schlecht wird...",
    delivery: "...dann war's wohl eine Brechstange.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 31,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Mir ist heute Morgen ein Joghurtbecher aus der Hand gefallen.",
    delivery: "Er war nicht mehr haltbar.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 32,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Im EDEKA ist gestern das Obstregal umgefallen.",
    delivery: "Es entstand ein Schaden in Melonenh\xF6he.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 33,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Gab es in Ihrer Familie jemals F\xE4lle von Inzest?",
    delivery: "Mitnichten!",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 34,
    safe: false
  },
  {
    category: "Pun",
    type: "single",
    joke: "Stauseen sind auch nur verdammte Fl\xFCsse.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 35,
    safe: true
  }
];
var jokes_de_default = {
  info: info2,
  jokes: jokes2
};

// netlify/functions/jokes/jokes-en.json
var info3 = {
  formatVersion: 3
};
var jokes3 = [
  {
    category: "Programming",
    type: "single",
    joke: "I've got a really good UDP joke to tell you but I don\u2019t know if you'll get it.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 0,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "How many programmers does it take to screw in a light bulb?",
    delivery: "None. It's a hardware problem.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 1,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: `A guy walks into a bar and asks for 1.4 root beers.
The bartender says "I'll have to charge you extra, that's a root beer float".
The guy says "In that case, better make it a double."`,
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 2,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "A programmer puts two glasses on his bedside table before going to sleep.\nA full one, in case he gets thirsty, and an empty one, in case he doesn't.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 3,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: `Java and C were telling jokes. It was C's turn, so he writes something on the wall, points to it and says "Do you get the reference?" But Java didn't.`,
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 4,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: 'A SQL statement walks into a bar and sees two tables.\nIt approaches, and asks "may I join you?"',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 5,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why did the web developer walk out of a resturant in disgust?",
    delivery: "The seating was laid out in tables.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 6,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why is 6 afraid of 7 in hexadecimal Canada?",
    delivery: "Because 7 8 9 A?",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 7,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Programming is like sex.",
    delivery: "Make one mistake and you end up supporting it for the rest of your life.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 8,
    safe: false
  },
  {
    category: "Programming",
    type: "single",
    joke: "Your mama's so FAT she can't save files bigger than 4GB.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 9,
    safe: false
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Hey, wanna hear a joke?",
    delivery: "Parsing HTML with regex.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 10,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why do programmers confuse Halloween and Christmas?",
    delivery: "Because Oct 31 = Dec 25",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 11,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "// This line doesn't actually do anything, but the code stops working when I delete it.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 12,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why does no one like SQLrillex?",
    delivery: "He keeps dropping the database.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 13,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "What do you call a developer who doesn't comment code?",
    delivery: "A developer.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 14,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why did the Python programmer not respond to the foreign mails he got?",
    delivery: "Because his interpreter was busy collecting garbage.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 15,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why did the programmer quit his job?",
    delivery: "Because he didn't get arrays.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 16,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "How can you tell an extroverted programmer?",
    delivery: "He looks at YOUR shoes when he's talking.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 17,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: `"Honey, go to the store and buy some eggs."
"OK."
"Oh and while you're there, get some milk."
He never returned.`,
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 18,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "How do you know God is a shitty programmer?",
    delivery: "He wrote the OS for an entire universe, but didn't leave a single useful comment.",
    flags: {
      nsfw: false,
      religious: true,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 19,
    safe: false
  },
  {
    category: "Programming",
    type: "single",
    joke: '"We messed up the keming again guys."',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 20,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why are modern programming languages so materialistic?",
    delivery: "Because they are object-oriented.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 21,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "If Bill Gates had a dime for every time Windows crashed ... Oh wait, he does.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 22,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "The glass is neither half-full nor half-empty, the glass is twice as big as it needs to be.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 23,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: `A byte walks into a bar looking miserable.
The bartender asks it: "What's wrong buddy?"
"Parity error." it replies. 
"Ah that makes sense, I thought you looked a bit off."`,
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 24,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "ASCII silly question, get a silly ANSI.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 25,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "What do you get if you lock a monkey in a room with a typewriter for 8 hours?",
    delivery: "A regular expression.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 26,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "Java is like Alzheimer's, it starts off slow, but eventually, your memory is gone.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 27,
    safe: false
  },
  {
    category: "Programming",
    type: "single",
    joke: `Two C strings walk into a bar.
The bartender asks "What can I get ya?"
The first string says "I'll have a gin and tonic."
The second string thinks for a minute, then says "I'll take a tequila sunriseJF()#$JF(#)$(@J#()$@#())!*FNIN!OBN134ufh1ui34hf9813f8h8384h981h3984h5F!##@"
The first string apologizes, "You'll have to excuse my friend, he's not null-terminated."`,
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 28,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "What do you call a group of 8 Hobbits?",
    delivery: "A Hobbyte.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 29,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "Saying that Java is nice because it works on every OS is like saying that anal sex is nice because it works on every gender.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 30,
    safe: false
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why did the JavaScript heap close shop?",
    delivery: "It ran out of memory.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 31,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "What is the best prefix for global variables?",
    delivery: "//",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 32,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: `Today I learned that changing random stuff until your program works is "hacky" and a "bad coding practice" but if you do it fast enough it's "Machine Learning" and pays 4x your current salary.`,
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 33,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: 'Eight bytes walk into a bar.\nThe bartender asks, "Can I get you anything?"\n"Yeah," reply the bytes.\n"Make us a double."',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 34,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "There are only 10 kinds of people in this world: those who know binary and those who don't.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 35,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: `"Knock, knock."
"Who's there?"

[very long pause]

"Java."`,
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 36,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "Programming is 10% science, 20% ingenuity, and 70% getting the ingenuity to work with the science.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 37,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: `A man is smoking a cigarette and blowing smoke rings into the air. His girlfriend becomes irritated with the smoke and says "Can't you see the warning on the cigarette pack? Smoking is hazardous to your health!" to which the man replies, "I am a programmer.  We don't worry about warnings; we only worry about errors."`,
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 38,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "The generation of random numbers is too important to be left to chance.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 39,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "Debugging: Removing the needles from the haystack.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 40,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "Hey Girl,\nRoses are #ff0000,\nViolets are #0000ff,\nI use hex codes,\nBut I'd use RGB for you.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 41,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "Debugging is like being the detective in a crime movie where you're also the murderer at the same time.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 42,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "How do you tell HTML from HTML5?\n- Try it out in Internet Explorer\n- Did it work?\n- No?\n- It's HTML5.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 43,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "Have a great weekend!\nI hope your code behaves the same on Monday as it did on Friday.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 44,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: 'Judge: "I sentence you to the maximum punishment..."\nMe (thinking): "Please be death, please be death..."\nJudge: "Learn Java!"\nMe: "Damn."',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 45,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "What's the object-oriented way to become wealthy?",
    delivery: "Inheritance.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 46,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why did the functional programmer get thrown out of school?",
    delivery: "Because he refused to take classes.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 48,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: ".NET developers are picky when it comes to food.",
    delivery: "They only like chicken NuGet.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 49,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why do programmers wear glasses?",
    delivery: "Because they need to C#",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 50,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "Algorithm: A word used by programmers when they don't want to explain how their code works.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 51,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "So what's a set of predefined steps the government might take to preserve the environment?",
    delivery: "An Al-Gore-ithm.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 52,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why do Java programmers hate communism?",
    delivery: "They don't want to live in a classless society.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 53,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Hey baby I wish your name was asynchronous...",
    delivery: "... so you'd give me a callback.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 54,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "Your momma is so fat, you need to switch to NTFS to store a picture of her.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 55,
    safe: false
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "What is a dying programmer's last program?",
    delivery: "Goodbye, world!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 56,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: `"Can I tell you a TCP joke?"
"Please tell me a TCP joke."
"OK, I'll tell you a TCP joke."`,
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 57,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "I asked my wife if I was the only one she's been with.",
    delivery: 'She said, "Yes, the others were at least sevens or eights."',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 58,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Thank you student loans for getting me through college.",
    delivery: "I don't think I'll ever be able to repay you.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 59,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Why is crucified Jesus always depicted with six-pack abs?",
    delivery: "He did CrossFit.",
    flags: {
      nsfw: false,
      religious: true,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 60,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "One time I masturbated on a plane.",
    delivery: 'I called it "highjacking".',
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 62,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "I used to love to tell dad jokes.",
    delivery: "Dad, come back...",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 63,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What do you call a cop's penis after he's done masturbating?",
    delivery: "Pulled pork.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 64,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Did you hear about the cheese factory that exploded in France?",
    delivery: "There was nothing but de brie.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 65,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "The average penis has...",
    delivery: "Been in and around my ex-girlfriend's mouth.\nFuck you, Karen!",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 66,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What kind of car did Whitney Houston drive?",
    delivery: "A Hyundaiiiiiiiiiiii",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 67,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What do you call crystal clear urine?",
    delivery: "1080p.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 69,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What happens when you don't obey the KGB?",
    delivery: "You get Putin jail.",
    flags: {
      nsfw: false,
      religious: false,
      political: true,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 70,
    safe: false
  },
  {
    category: "Misc",
    type: "single",
    joke: "Two reasons I don't give money to homeless people.\n1) They are going to spend it all on drugs and alcohol\n2) I am going to spend it all on drugs and alcohol.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 72,
    safe: false
  },
  {
    category: "Misc",
    type: "single",
    joke: 'A neutron walks into a bar and asks for a price on a drink.\nThe barkeeper says: "For you... no charge!"',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 73,
    safe: true
  },
  {
    category: "Pun",
    type: "single",
    joke: 'A horse walks into a bar.\n"Hey", the Bartender says.\n"Sure", the horse replies.',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 74,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "My girlfriend left me because I have a fetish for touching pasta.",
    delivery: "I'm feeling cannelloni now. :'(",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 75,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What is the least spoken language in the world?",
    delivery: "Sign language.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 76,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "I WRITE MY JOKES IN CAPITALS.",
    delivery: "THIS ONE WAS WRITTEN IN PARIS.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 78,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "So I made a graph of all my past relationships.",
    delivery: "It has an ex axis and a why axis.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 80,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "My wife left me because I'm too insecure and paranoid.",
    delivery: "Oh wait, never mind. She was just getting the mail.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 81,
    safe: true
  },
  {
    category: "Pun",
    type: "single",
    joke: 'I have these weird muscle spasms in my gluteus maximus.\nI figured out from my doctor that everything was alright:\nHe said "Weird flex, butt okay."',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 82,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "Went to the doctors for a prostate exam.\nDuring the exam he said it's not unusual to become aroused or even ejaculate.",
    delivery: "But still, I wish he hadn't.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 83,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "If I make you breakfast in bed, a simple thank you is all I need.",
    delivery: 'Not all this "How the fuck did you get in my house?!" nonsense.',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 84,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Why did the Romanian stop reading?",
    delivery: "They wanted to give the Bucharest.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 86,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "I hate Russian matryoshka dolls.",
    delivery: "They're so full of themselves.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 87,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "Mom asked me where I'm taking her to go out to eat for mother's day.",
    delivery: 'I told her, "We already have food in the house".',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 88,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What's green and smells like pork?",
    delivery: "Kermit's Fingers.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 89,
    safe: false
  },
  {
    category: "Pun",
    type: "single",
    joke: "Oysters hate to give away their pearls because they are shellfish.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 90,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "How many nice guys does it take to change a lightbulb?",
    delivery: "None, they'll just compliment it and get pissed off when it won't screw.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 91,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What's the difference between an in-law and an outlaw?",
    delivery: "An outlaw is wanted.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 92,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: `My mother said, "You won't amount to anything because you always procrastinate."`,
    delivery: 'I said, "Oh yeah... Just you wait."',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 94,
    safe: true
  },
  {
    category: "Misc",
    type: "single",
    joke: "Stop being homophobic and rude to the LGBTQ+ community. You should be thanking them for saving us plenty of room in heaven.",
    flags: {
      nsfw: false,
      religious: true,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 114,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "The six stages of debugging:\n1. That can't happen.\n2. That doesn't happen on my machine.\n3. That shouldn't happen.\n4. Why does that happen?\n5. Oh, I see.\n6. How did that ever work?",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 123,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What time did the man go to the dentist?",
    delivery: "Tooth hurt-y.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 125,
    safe: true
  },
  {
    category: "Pun",
    type: "single",
    joke: "I'm reading a book about anti-gravity. It's impossible to put down!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 126,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "How do you generate a random string?",
    delivery: "Put a Windows user in front of Vim and tell them to exit.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 127,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What do you call a pile of kittens?",
    delivery: "A meowntain.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 130,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "How does a Jewish person make tea?",
    delivery: "Hebrews it.",
    flags: {
      nsfw: false,
      religious: true,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 131,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: `Four engineers get into a car. The car won't start.
The Mechanical engineer says "It's a broken starter".
The Electrical engineer says "Dead battery".
The Chemical engineer says "Impurities in the gasoline".
The IT engineer says "Hey guys, I have an idea: How about we all get out of the car and get back in".`,
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 132,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Why do Hong Kong cops like to go to work early?",
    delivery: "To beat the crowd.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 136,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What does tofu and a dildo have in common?",
    delivery: "They're both meat substitutes.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 137,
    safe: false
  },
  {
    category: "Misc",
    type: "single",
    joke: "If you're here for the yodeling lesson, please form an orderly orderly orderly queue.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 139,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What kind of doctor is Dr. Pepper?",
    delivery: "He's a fizzician.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 140,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What's the difference between a hot potato and a flying pig?",
    delivery: "One's a heated yam, the other's a yeeted ham.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 141,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What's the difference between a feminist and a grenade?",
    delivery: "The grenade actually accomplishes something when it triggers.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: true,
      explicit: false
    },
    id: 142,
    safe: false
  },
  {
    category: "Dark",
    type: "twopart",
    setup: "My neighbor is a 90 year old with Alzheimer's, I see him every morning and he asks me If I've seen his wife.\nEvery day I have to tell this poor man that his wife died 20 years ago.\nI could have moved to another house or even ignore his question.",
    delivery: "But the look of joy in his eyes whenever I answer him is worth the world.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 145,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "This morning I accidentally made my coffee with Red Bull instead of water.",
    delivery: "I was already on the highway when I noticed I forgot my car at home.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 146,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "How did you make your friend rage?",
    delivery: "I implemented a greek question mark in his JavaScript code.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 147,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: 'The gas Argon walks into a bar.\nThe barkeeper says "What would you like to drink?"',
    delivery: "But Argon doesn't react.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 148,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What do you call a deaf gynecologist?",
    delivery: "A lip reader.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 155,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What do you call a cheap circumcision?",
    delivery: "A rip off.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 156,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What does a woman's pussy and a chainsaw have in common?",
    delivery: "Miss by a few inches and you're in deep shit.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 157,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What's the difference between a gay guy and a refrigerator?",
    delivery: "A refrigerator doesn't fart when I pull my meat out of it.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 163,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "I'm sure good looking lesbians look at fat lesbians and give them no chance.",
    delivery: "Until they see their fingers.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 164,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "I like my girls how I like my COVID.",
    delivery: "19 and easily spread.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 167,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Two guys walked into a bar.",
    delivery: "The third guy ducked.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 168,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Two peanuts were walking.",
    delivery: "One was assaulted.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 169,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What kind of bees produce milk?",
    delivery: "Boo-Bees.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 170,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "A grocery store cashier asked if I would like my milk in a bag.",
    delivery: 'I told her "No, thanks. The carton works fine".',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 171,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What does a perverted frog say?",
    delivery: "Rubbit.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 172,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What is the difference between an oral thermometer and a rectal thermometer?",
    delivery: "The taste.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 175,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What do you call a witch at the beach?",
    delivery: "A sandwich.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 176,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What do you call 4 Mexicans in quicksand?",
    delivery: "Quatro Sinko.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 177,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "Knock knock.\nWho's there?\nRecursion.\nRecursion who?\nKnock knock.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 180,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What is the difference between the Constitutions of the USA and the USSR? Don't both of them guarantee freedom of speech?",
    delivery: "Yes, but the Constitution of the USA also guarantees freedom after the speech.",
    flags: {
      nsfw: false,
      religious: false,
      political: true,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 181,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "Why was the river rich?",
    delivery: "Because it had two banks.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 182,
    safe: true
  },
  {
    category: "Spooky",
    type: "twopart",
    setup: "Why didn't the skeleton go for prom?",
    delivery: "Because it had nobody.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 183,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: "Being a self-taught developer is almost the same as being a cut neck chicken because you have no sense of direction in the beginning.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 184,
    safe: false
  },
  {
    category: "Pun",
    type: "single",
    joke: 'Two fish in a tank. One turns to the other and says, "Do you know how to drive this thing?"',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 185,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "I told my wife to shave her pussy.",
    delivery: "I woke up bald.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 187,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What's the difference between a school bus and a cactus?",
    delivery: "A cactus keeps the little pricks on the outside.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 188,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What did the customer say to the waiter?",
    delivery: "I'm all fed up with your service.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 190,
    safe: true
  },
  {
    category: "Pun",
    type: "single",
    joke: "To whoever stole my copy of Microsoft Office, I will find you. You have my Word!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 191,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Why did the koala get rejected?",
    delivery: "Because he did not have any koalafication.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 192,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "What is the most used language in programming?",
    delivery: "Profanity.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 193,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What's the best thing about Switzerland?",
    delivery: "I don't know, but the flag is a big plus.",
    flags: {
      nsfw: false,
      religious: false,
      racist: false,
      sexist: false,
      political: false,
      explicit: false
    },
    id: 194,
    safe: true
  },
  {
    category: "Misc",
    type: "single",
    joke: "Relationship Status: just tried to reach for my dog's paw and he pulled it away so I pretended I was reaching for the remote.",
    flags: {
      nsfw: false,
      religious: false,
      racist: false,
      sexist: false,
      political: false,
      explicit: false
    },
    id: 195,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What do you call a cow with no legs?",
    delivery: "Ground beef.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 196,
    safe: true
  },
  {
    category: "Misc",
    type: "single",
    joke: "Schr\xF6dinger's cat walks into a bar and doesn't.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 197,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "The past, the present and the future walk into a bar.",
    delivery: "It was tense.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 198,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What did the cell say when his sister cell stepped on his foot?",
    delivery: "Mitosis.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 199,
    safe: true
  },
  {
    category: "Pun",
    type: "single",
    joke: `Today, my son asked "Can I have a book mark?" and I burst into tears.
11 years old and he still doesn't know my name is Brian.`,
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 200,
    safe: true
  },
  {
    category: "Misc",
    type: "single",
    joke: "My wife is really mad at the fact that I have no sense of direction.\nSo I packed up my stuff and right.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 201,
    safe: true
  },
  {
    category: "Pun",
    type: "single",
    joke: "How do you make holy water? You boil the hell out of it.",
    flags: {
      nsfw: false,
      religious: true,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 202,
    safe: false
  },
  {
    category: "Pun",
    type: "single",
    joke: "How do you make holy water? You freeze it and drill holes in it.",
    flags: {
      nsfw: false,
      religious: true,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 203,
    safe: false
  },
  {
    category: "Pun",
    type: "single",
    joke: "I bought some shoes from a drug dealer. I don't know what he laced them with, but I was tripping all day!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 204,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What did the fish say when it swam into the wall?",
    delivery: "Dam.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 205,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "How did Harry Potter get down the hill?",
    delivery: "Walking...\nJK, Rolling.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 206,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Why did the chicken cross the road, roll in the mud and cross the road again?",
    delivery: "He was a dirty double-crosser!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 209,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What do you call a deer with no eyes?",
    delivery: "No eye deer.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 210,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "What are bits?",
    delivery: "Tiny things left when you drop your computer down the stairs.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 211,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Where do sick cruise ships go to get healthy?",
    delivery: "The dock!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 212,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Has COVID-19 forced you to wear glasses and a mask at the same time?",
    delivery: "If so, you may be entitled to condensation.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 213,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why did the programmer jump on the table?",
    delivery: "Because debug was on his screen.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 214,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "I walked into a bar once.",
    delivery: "It really hurt my head.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 215,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What's grey and comes in pints?",
    delivery: "An elephant.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 216,
    safe: false
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "How do construction workers party?",
    delivery: "They raise the roof.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 217,
    safe: true
  },
  {
    category: "Pun",
    type: "single",
    joke: "I was struggling to figure out how lightning works, but then it struck me.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 220,
    safe: true
  },
  {
    category: "Programming",
    type: "single",
    joke: 'Two SQL tables sit at the bar. A query approaches and asks "Can I join you?"',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 221,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Why do cows wear bells?",
    delivery: "Because their horns don't work!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 222,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "Why was the mushroom always invited to parties?",
    delivery: "Cause he's a fungi.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 223,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What do you call a bird sitting with their legs spread?",
    delivery: "A prostitweety.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 224,
    safe: false
  },
  {
    category: "Pun",
    type: "twopart",
    setup: `Mickey Mouse and Minnie Mouse are in the divorce court.
"Mickey", the judge says, "I'm sorry. I can't grant you a divorce on the grounds of insanity. Minnie seems quite sane to me."`,
    delivery: `"I didn't say she was insane", exclaims Mickey.
"I said she was fucking Goofy."`,
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 225,
    safe: false
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why do they call it hyper terminal?",
    delivery: "Too much Java.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 226,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "How much did your chimney cost?",
    delivery: "Nothing, it was on the house.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 231,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why do programmers prefer using the dark mode?",
    delivery: "Because light attracts bugs.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 232,
    safe: true
  },
  {
    category: "Misc",
    type: "single",
    joke: "What does the MacBook have in common with Donald Trump?\n\nI would tell you....\nBut I don't compare apples to oranges.",
    flags: {
      nsfw: false,
      religious: false,
      political: true,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 233,
    safe: false
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why did the Python data scientist get arrested at customs?",
    delivery: "She was caught trying to import pandas!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 234,
    safe: true
  },
  {
    category: "Misc",
    type: "single",
    joke: "My parents raised me as an only child, which really annoyed my younger brother.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 235,
    safe: true
  },
  {
    category: "Pun",
    type: "single",
    joke: "I was reading a great book about an immortal dog the other day. It was impossible to put down.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 236,
    safe: true
  },
  {
    category: "Pun",
    type: "single",
    joke: "I have a joke about trickle down economics, but 99% of you will never get it.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 238,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What's long and hard and has cum in it?",
    delivery: "A cucumber.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    id: 240,
    safe: false
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "What does Santa suffer from if he gets stuck in a chimney?",
    delivery: "Claustrophobia!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 241,
    safe: true
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "Why does Santa have three gardens?",
    delivery: 'So he can "ho ho ho"!',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 242,
    safe: true
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "Why did Santa's helper see the doctor?",
    delivery: "Because he had a low elf-esteem.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 243,
    safe: true
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "What kind of motorbike does Santa ride?",
    delivery: "A Holly Davidson!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 244,
    safe: true
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: 'What says "Oh Oh Oh"?',
    delivery: "Santa walking backwards.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 245,
    safe: true
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "Who is Santa's favourite singer?",
    delivery: "Elf-is Presley.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 246,
    safe: true
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "What's Santa's favourite type of music?",
    delivery: "Wrap!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 247,
    safe: true
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "What do Santa's little helpers learn at school?",
    delivery: "The elf-abet.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 248,
    safe: true
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "What do elves post on Social Media?",
    delivery: "Elf-ies!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 249,
    safe: true
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "Who hides in the bakery at Christmas?",
    delivery: "A mince spy!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 250,
    safe: true
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "How will Christmas dinner be different after Brexit?",
    delivery: "No Brussels!",
    flags: {
      nsfw: false,
      religious: false,
      political: true,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 251,
    safe: false
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "Why couldn't the skeleton go to the Christmas party?",
    delivery: "Because he had no body to go with!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 252,
    safe: true
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "Why does Santa go down the chimney?",
    delivery: "Because it soots him!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 253,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Why do front end developers eat lunch alone?",
    delivery: "Because they don't know how to join tables.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 254,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "Why are cats so good at video games?",
    delivery: "They have nine lives.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 255,
    safe: true
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Why did the banana go see a doctor?",
    delivery: "Because it wasn't peeling well.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 256,
    safe: true
  },
  {
    category: "Misc",
    type: "single",
    joke: "My wife and I have reached the difficult decision that we do not want children.\nIf anybody does, please just send me your contact details and we can drop them off tomorrow.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 257,
    safe: true
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "I won the lottery today!",
    delivery: "Well, I only got the first two numbers, but my lawyers are working on having them stop the count.",
    flags: {
      nsfw: false,
      religious: false,
      political: true,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 258,
    safe: false
  },
  {
    category: "Programming",
    type: "single",
    joke: "UDP is better in the COVID era since it avoids unnecessary handshakes.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    id: 259,
    safe: true
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why is Linux safe?",
    delivery: "Hackers peak through Windows only.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 260
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why are Assembly programmers always soaking wet?",
    delivery: "They work below C-level.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 264
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why did the database administrator leave his wife?",
    delivery: "She had one-to-many relationships.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 265
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "How did the programmer die in the shower?",
    delivery: "He read the shampoo bottle instructions: Lather. Rinse. Repeat.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 266
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What's the difference between a poorly dressed man on a unicycle and a well dressed man on a bicycle?",
    delivery: "Attire.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 267
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Why shouldn't you visit an expensive wig shop?",
    delivery: `It's too high a price "toupee."`,
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 268
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "You see, mountains aren't just funny.",
    delivery: "They are hill areas.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 270
  },
  {
    category: "Misc",
    type: "single",
    joke: "Never date a baker. They're too kneady.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 271
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What does the mermaid wear to math class?",
    delivery: "Algae-bra.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: false,
    id: 272
  },
  {
    category: "Misc",
    type: "single",
    joke: "My husband and I were happy for 20 years. And then we met.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 273
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Did you hear about the claustrophobic astronaut?",
    delivery: "He just needed a little space.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 276
  },
  {
    category: "Misc",
    type: "twopart",
    setup: '"99.9% of the people are dumb!"',
    delivery: '"Fortunately I belong to the remaining 1%"',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 277
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "No matter how kind you are...",
    delivery: "German kids are always Kinder.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 278
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Which is faster, hot or cold?",
    delivery: "Hot, because you can catch a cold.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 279
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "To prove he was right, the flat-earther walked to the end of the earth.",
    delivery: "He eventually came around.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 280
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "I just saw my wife trip over and drop a basket full of ironed clothes.",
    delivery: "I watched it all unfold.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 281
  },
  {
    category: "Pun",
    type: "twopart",
    setup: 'I was feeling depressed, my wife put her hand on my back and said "Earth."',
    delivery: "It meant the world to me.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 282
  },
  {
    category: "Pun",
    type: "twopart",
    setup: 'My employer came running to me and said, "I was looking for you all day! Where the hell have you been?"',
    delivery: 'I replied, "Good employees are hard to find."',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 283
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "The other day my wife asked me to pass her lipstick, but I accidentally gave her a glue stick.",
    delivery: "She still isn't talking to me.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 284
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Which part of the hospital has the least privacy?",
    delivery: "The ICU.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 285
  },
  {
    category: "Pun",
    type: "single",
    joke: 'A Roman walks into a bar, raises 2 fingers and says to the bartender "five beers, please."',
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 286
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "I stayed up all night wondering where the sun went.",
    delivery: "Then it dawned on me.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 287
  },
  {
    category: "Misc",
    type: "single",
    joke: "A perfectionist walked into a bar... apparently, the bar was not set high enough.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 288
  },
  {
    category: "Misc",
    type: "single",
    joke: "In Soviet Russia, gay sex gets you arrested. In America, getting arrested gets you gay sex.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: false,
    id: 289
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Why does the size of the snack not matter to a giraffe?",
    delivery: "Because even a little bit goes a long way.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 290
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "Why was the Javascript developer sad?",
    delivery: "Because they didn't Node how to Express themself.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 292
  },
  {
    category: "Christmas",
    type: "twopart",
    setup: "Whats the Grinchs least favorite band?",
    delivery: "The Who.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 293
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "why do python programmers wear glasses?",
    delivery: "Because they can't C.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 294
  },
  {
    category: "Spooky",
    type: "twopart",
    setup: "Why do ghosts go on diets?",
    delivery: "So they can keep their ghoulish figures.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 295
  },
  {
    category: "Spooky",
    type: "twopart",
    setup: "What is in a ghost's nose?",
    delivery: "Boo-gers.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 296
  },
  {
    category: "Spooky",
    type: "twopart",
    setup: "What's it like to be kissed by a vampire?",
    delivery: "It's a pain in the neck.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 297
  },
  {
    category: "Spooky",
    type: "twopart",
    setup: "What does a turkey dress up as for Halloween?",
    delivery: "A gobblin'!",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 298
  },
  {
    category: "Spooky",
    type: "twopart",
    setup: "Why did the ghost go inside the bar?",
    delivery: "For the boos.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 299
  },
  {
    category: "Spooky",
    type: "twopart",
    setup: "How do Rednecks celebrate Halloween?",
    delivery: "Pump kin!",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: false,
    id: 300
  },
  {
    category: "Programming",
    type: "single",
    joke: "I have a joke about Stack Overflow, but you would say it's a duplicate.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 301
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "I just got fired from my job at the keyboard factory.",
    delivery: "They told me I wasn't putting in enough shifts.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 302
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "I can't believe I got fired from the calendar factory.",
    delivery: "All I did was take a day off.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 303
  },
  {
    category: "Misc",
    type: "single",
    joke: "I visited my friend at his new house. He told me to make myself at home. So I threw him out. I hate having visitors.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 304
  },
  {
    category: "Programming",
    type: "single",
    joke: "Documentation is like sex:\nWhen it's good, it's very good.\nWhen it's bad, it's better than nothing...",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: false,
    id: 305
  },
  {
    category: "Misc",
    type: "single",
    joke: "Yo mama is so old, she knew Burger King while he was still a prince.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 306
  },
  {
    category: "Misc",
    type: "single",
    joke: "I went to the zoo the other day. There was only a dog in it \u2013 it was a shihtzu.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    safe: false,
    id: 307
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "What's the difference between England and a tea bag?",
    delivery: "The tea bag stays in the cup longer.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 308
  },
  {
    category: "Spooky",
    type: "twopart",
    setup: "What happened to the cannibal who showed up late to Halloween dinner?",
    delivery: "They gave him the cold shoulder.",
    flags: {
      nsfw: true,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: false,
    id: 311
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Do you know what killed the man who had a two ton pumpkin fall on him?",
    delivery: "He was squashed.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 312
  },
  {
    category: "Spooky",
    type: "twopart",
    setup: "I'm not saying my son is ugly...",
    delivery: "But on Halloween he went to tell the neighbors to turn down their TV and they gave him some candy.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 313
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What happened to the man who got behind on payments to his exorcist?",
    delivery: "He got repossessed.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 314
  },
  {
    category: "Spooky",
    type: "twopart",
    setup: "Why did the ghost go to the bar?",
    delivery: "To get sheet faced.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: true
    },
    safe: false,
    id: 315
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Did you hear about the crime in the parking garage?",
    delivery: "It was wrong on so many levels.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 316
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "What do you call a caveman's fart?",
    delivery: "A blast from the past.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 317
  },
  {
    category: "Pun",
    type: "twopart",
    setup: "Why should you never talk to pi?",
    delivery: "Because it will go on forever.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 318
  },
  {
    category: "Programming",
    type: "single",
    joke: "I'd tell you a joke about NAT but I would have to translate.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 319
  }
];
var jokes_en_default = {
  info: info3,
  jokes: jokes3
};

// netlify/functions/jokes/jokes-es.json
var info4 = {
  formatVersion: 3
};
var jokes4 = [
  {
    category: "Misc",
    type: "single",
    joke: 'Una ardilla le pregunta a otra que quiere ser cuando grande, la otra ardilla de responde "Ardila L\xFClle".',
    flags: {
      nsfw: true,
      religious: false,
      political: true,
      racist: false,
      sexist: false
    },
    id: 0,
    safe: false
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "\xBFQu\xE9 es un terapeuta?",
    delivery: "1024 Gigapeutas",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 1
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "\xBFQu\xE9 le dice un .GIF a un .JPEG?",
    delivery: "An\xEDmate viejo.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 2
  },
  {
    category: "Programming",
    type: "single",
    joke: "No te despedir\xE1n del trabajo, si nunca comentas tu c\xF3digo y adem\xE1s eres el \xFAnico que sabe c\xF3mo funciona",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 3
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "\xBFQu\xE9 es el hardware?",
    delivery: "El que recibe los golpes cuando falla el software.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 4
  },
  {
    category: "Programming",
    type: "single",
    joke: "S\xF3lo hay 10 tipos de personas en este mundo, las que entienden binario y las que no.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 5
  },
  {
    category: "Programming",
    type: "twopart",
    setup: "\xBFPor qu\xE9 C consigue todas las chicas y Java no tiene ninguna?",
    delivery: "Porque C no las trata como objetos.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 6
  }
];
var jokes_es_default = {
  info: info4,
  jokes: jokes4
};

// netlify/functions/jokes/jokes-fr.json
var info5 = {
  formatVersion: 3
};
var jokes5 = [];
var jokes_fr_default = {
  info: info5,
  jokes: jokes5
};

// netlify/functions/jokes/jokes-pt.json
var info6 = {
  formatVersion: 3
};
var jokes6 = [
  {
    category: "Misc",
    type: "twopart",
    setup: "O que \xE9 um cheiro verde?",
    delivery: "Peido do Hulk.",
    flags: {
      nsfw: false,
      religious: false,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: true,
    id: 0
  },
  {
    category: "Misc",
    type: "twopart",
    setup: "O que o pagodeiro foi fazer na igreja?",
    delivery: "Cantar p\xE1 God.",
    flags: {
      nsfw: false,
      religious: true,
      political: false,
      racist: false,
      sexist: false,
      explicit: false
    },
    safe: false,
    id: 1
  }
];
var jokes_pt_default = {
  info: info6,
  jokes: jokes6
};

// netlify/functions/joke.ts
var jokes7 = [
  ...jokes_cs_default.jokes.map((joke) => __spreadValues({ lang: "cs" }, joke)),
  ...jokes_de_default.jokes.map((joke) => __spreadValues({ lang: "de" }, joke)),
  ...jokes_en_default.jokes.map((joke) => __spreadValues({ lang: "en" }, joke)),
  ...jokes_es_default.jokes.map((joke) => __spreadValues({ lang: "es" }, joke)),
  ...jokes_fr_default.jokes.map((joke) => __spreadValues({ lang: "fr" }, joke)),
  ...jokes_pt_default.jokes.map((joke) => __spreadValues({ lang: "pt" }, joke))
];
if (!Array.prototype.random) {
  Array.prototype.random = function random() {
    const r = Math.floor(Math.random() * this.length);
    return this[r];
  };
}
if (!Array.prototype.intersect) {
  Array.prototype.intersect = function intersect(another) {
    const array1 = this.length > another.length ? this : another;
    const array2 = this.length > another.length ? another : this;
    const filteredArray = array1.filter((value) => array2.includes(value));
    return filteredArray;
  };
}
var handler = async (event, context) => {
  var _a, _b, _c, _d, _e, _f, _g, _h, _i, _j;
  let filteredJokes = jokes7;
  const idRange = (_b = (_a = event.multiValueQueryStringParameters) == null ? void 0 : _a.idRange) == null ? void 0 : _b.flatMap((d) => d.split(",")).map(parseInt);
  if (idRange) {
    console.log("idRange");
    if (idRange.length === 1) {
      filteredJokes = [filteredJokes.find((d) => d.id === idRange[0])];
    } else if (idRange.length === 2) {
      const [from, to] = idRange;
      filteredJokes = filteredJokes.filter((d) => d.id >= from && d.id <= to);
    }
  }
  const lang = (_c = event.queryStringParameters) == null ? void 0 : _c.lang;
  if (lang) {
    console.log("lang");
    filteredJokes = filteredJokes.filter((d) => d.lang === lang);
  }
  const safe = (_d = event.queryStringParameters) == null ? void 0 : _d.safe;
  if (safe) {
    const s = safe !== "false";
    filteredJokes = filteredJokes.filter((d) => d.safe === s);
  }
  const allowedTypes = (_f = (_e = event.multiValueQueryStringParameters) == null ? void 0 : _e.type) == null ? void 0 : _f.flatMap((d) => d.split(","));
  if (allowedTypes) {
    console.log(allowedTypes);
    filteredJokes = filteredJokes.filter((d) => allowedTypes.includes(d.type));
  }
  const allowedCategories = (_h = (_g = event.multiValueQueryStringParameters) == null ? void 0 : _g.categories) == null ? void 0 : _h.flatMap((d) => d.split(","));
  if (allowedCategories) {
    console.log(allowedCategories);
    filteredJokes = filteredJokes.filter((d) => allowedCategories.includes(d.category));
  }
  const blacklistedFlags = (_j = (_i = event.multiValueQueryStringParameters) == null ? void 0 : _i.flags) == null ? void 0 : _j.flatMap((d) => d.split(","));
  if (blacklistedFlags) {
    console.log(blacklistedFlags);
    filteredJokes = filteredJokes.filter((d) => d.safe === false).filter((d) => {
      const jokeFlags = Object.entries(d.flags).filter(([, active]) => active).map((d2) => d2[0]);
      return jokeFlags.intersect(blacklistedFlags).length === 0;
    });
  }
  return {
    statusCode: 200,
    headers: {
      "Content-Type": "application/json; charset=utf-8",
      "Access-Control-Allow-Origin": "*"
    },
    body: JSON.stringify(filteredJokes.random())
  };
};
module.exports = __toCommonJS(joke_exports);
// Annotate the CommonJS export names for ESM import in node:
0 && (module.exports = {
  handler
});
//# sourceMappingURL=joke.js.map
