import { Elm } from '../elm/KJoker.elm';

const app = Elm.KJoker.init({
  node: document.querySelector('main'),
});

console.log(app.ports);
app.ports.speechSynthesis.subscribe(({ text, lang }) => {
  console.log('speaking', text);
  const s = new SpeechSynthesisUtterance(text);
  s.lang = lang;
  speechSynthesis.speak(s);
});
