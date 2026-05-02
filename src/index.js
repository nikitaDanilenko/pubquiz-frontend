import './main.css';
import { Elm } from './Main.elm';

const getInitialTheme = () => {
  const stored = localStorage.getItem('theme');
  if (stored) return stored;
  return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light';
};

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    apiBase: window.CONFIG?.apiBase || '',
    theme: getInitialTheme()
  }
});

app.ports.saveTheme.subscribe((theme) => {
  localStorage.setItem('theme', theme);
});
