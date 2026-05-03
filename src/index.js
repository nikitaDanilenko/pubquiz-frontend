import './main.css';
import { Elm } from './Main.elm';

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
    theme: initialTheme
  }
});

app.ports.saveTheme.subscribe((theme) => {
  localStorage.setItem('theme', theme);
  document.documentElement.setAttribute('data-theme', theme);
});
