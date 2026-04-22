export const setHtml = (selector) => (html) => () => {
  const el = document.querySelector(selector);
  if (!el) throw new Error("setHtml: no element matches " + selector);
  el.innerHTML = html;
};
