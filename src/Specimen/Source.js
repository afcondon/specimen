export const _fetchText = (url) => () =>
  fetch(url).then((res) => {
    if (!res.ok) throw new Error("fetchText: " + res.status + " " + url);
    return res.text();
  });

export const _queryParam = (name) => () => {
  const params = new URLSearchParams(window.location.search);
  return params.get(name);
};
