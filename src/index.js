import './main.css';
import { Elm } from './Main.elm';

/**
 * There is a tricky underlying issue in the pubquiz service:
 * Both the back end and the front end provide routes for the same paths, e.g. `backoffice`.
 * Since both ends do not explicitly know the domain they are running on,
 * they are referring to `/backoffice/<something>`.
 * From their own perspective, everything is fine,
 * because the correct path gets resolved by standard behaviour.
 * However, when both run together there is an issue:
 * What does `<domain>/backoffice/<something>` resolve to?
 * There is no clean way of knowing via the path alone.
 * This ambiguity is resolved, by prefixing the back end domain on deployment level,
 * i.e. the deployment infrastructure routes all requests from "/<back-end-prefix>/*" to the back end.
 * The back end service itself (i.e. in the code) does not know about this extra prefix,
 * which is good, since it is not relevant for that area.
 * The front end should behave similarly, but there is no clean way of doing this via deployment alone:
 * The front end could use the paths `<domain>/backoffice/<something>` for both a back end request,
 * and a front end link.
 * Here is where this trick comes in.
 * We use a runtime variable for the back end prefix.
 * It is set as "$BACK_END_PREFIX" in the `index.html` file (note the leading $ sign),
 * and as the correct variable from the `values.yaml` for the actual deployment.
 * Here, we use the following convention:
 * 1. If the variable is starting with $, e.g. "$BACK_END_PREFIX",
 *    then no deployment substitution took place, meaning that we are in development.
 *    In that case, we use an empty prefix, and use the plain back end address.
 * 2. Otherwise, the variable is set, and every back end request gets silently prefixed with the actual prefix.
 *    In the production example for this service, this means that the prefix is "/api".
 *
 * The technique is called "XHR interception", and relies on assumptions,
 * mostly that Elm is using XHR (XMLHttpRequests) internally.
 * The approach is hacky, because it is not quite as visible as the deployment configuration for the back end,
 * and is only used as a pragmatic compromise.
 * A cleaner approach would use a real prefix, and pass it through every call,
 * but that does not work as nicely with the OpenAPI code generation,
 * and propagates through all requests in the code.
 * In principle, even the prefix approach is also somewhat of a compromise,
 * because there is no hard requirement that the back end and front end run under the same domain!
 */
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
