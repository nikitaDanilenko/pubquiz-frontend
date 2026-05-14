import './main.css';
import { Elm } from './Main.elm';

const raw = window.__BACK_END_PREFIX__ || "";
const backEndPrefix = raw.startsWith("$") ? "" : raw;
if (backEndPrefix) {
  const origOpen = XMLHttpRequest.prototype.open;
  XMLHttpRequest.prototype.open = function (method, url, ...rest) {
    return origOpen.call(this, method, backEndPrefix + url, ...rest);
  };
}

const getInitialTheme = () => {
  const stored = localStorage.getItem('theme');
  if (stored) return stored;
  return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
};

const initialTheme = getInitialTheme();
document.documentElement.setAttribute('data-theme', initialTheme);

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    theme: initialTheme,
    baseUrl: window.location.origin
  }
});

app.ports.saveTheme.subscribe((theme) => {
  localStorage.setItem('theme', theme);
  document.documentElement.setAttribute('data-theme', theme);
});
