import { Handler } from '@netlify/functions';
import jokesCs from './jokes/jokes-cs.json';
import jokesDe from './jokes/jokes-de.json';
import jokesEn from './jokes/jokes-en.json';
import jokesEs from './jokes/jokes-es.json';
import jokesFr from './jokes/jokes-fr.json';
import jokesPt from './jokes/jokes-pt.json';

const jokes = [
  ...jokesCs.jokes.map((joke) => ({ lang: 'cs', ...joke })),
  ...jokesDe.jokes.map((joke) => ({ lang: 'de', ...joke })),
  ...jokesEn.jokes.map((joke) => ({ lang: 'en', ...joke })),
  ...jokesEs.jokes.map((joke) => ({ lang: 'es', ...joke })),
  ...jokesFr.jokes.map((joke) => ({ lang: 'fr', ...joke })),
  ...jokesPt.jokes.map((joke) => ({ lang: 'pt', ...joke })),
];

declare global {
  interface Array<T> {
    random(): T;
    intersect(another: T[]): T[];
  }
}

if (!Array.prototype.random) {
  Array.prototype.random = function random<T>(this: T[]): T {
    const r = Math.floor(Math.random() * this.length);
    return this[r];
  };
}

if (!Array.prototype.intersect) {
  Array.prototype.intersect = function intersect<T>(
    this: T[],
    another: T[],
  ): T[] {
    const array1 = this.length > another.length ? this : another;
    const array2 = this.length > another.length ? another : this;
    const filteredArray = array1.filter((value) => array2.includes(value));
    return filteredArray;
  };
}

const handler: Handler = async (event, context) => {
  let filteredJokes = jokes;

  const idRange = event.multiValueQueryStringParameters?.idRange
    ?.flatMap((d) => d.split(','))
    .map(parseInt);
  if (idRange) {
    console.log('idRange');
    if (idRange.length === 1) {
      filteredJokes = [filteredJokes.find((d) => d.id === idRange[0])];
    } else if (idRange.length === 2) {
      const [from, to] = idRange;
      filteredJokes = filteredJokes.filter((d) => d.id >= from && d.id <= to);
    }
  }

  const lang = event.queryStringParameters?.lang;
  if (lang) {
    console.log('lang');
    filteredJokes = filteredJokes.filter((d) => d.lang === lang);
  }

  const safe = event.queryStringParameters?.safe;
  if (safe) {
    const s = safe !== 'false';
    filteredJokes = filteredJokes.filter((d) => d.safe === s);
  }

  const allowedTypes = event.multiValueQueryStringParameters?.type?.flatMap(
    (d) => d.split(','),
  );
  if (allowedTypes) {
    console.log(allowedTypes);
    filteredJokes = filteredJokes.filter((d) => allowedTypes.includes(d.type));
  }

  const allowedCategories =
    event.multiValueQueryStringParameters?.categories?.flatMap((d) =>
      d.split(','),
    );
  if (allowedCategories) {
    console.log(allowedCategories);
    filteredJokes = filteredJokes.filter((d) =>
      allowedCategories.includes(d.category),
    );
  }

  const blacklistedFlags =
    event.multiValueQueryStringParameters?.flags?.flatMap((d) => d.split(','));
  if (blacklistedFlags) {
    console.log(blacklistedFlags);
    filteredJokes = filteredJokes
      .filter((d) => d.safe === false)
      .filter((d) => {
        const jokeFlags = Object.entries(d.flags)
          .filter(([, active]) => active)
          .map((d) => d[0]);
        return jokeFlags.intersect(blacklistedFlags).length === 0;
      });
  }

  return {
    statusCode: 200,
    headers: {
      'Content-Type': 'application/json; charset=utf-8',
      'Access-Control-Allow-Origin': '*',
    },
    body: JSON.stringify(filteredJokes.random()),
  };
};

export { handler };
