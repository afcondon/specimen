const SECTIONS = "[data-shelf]";

export const readShelves = () => {
  const payload = document.getElementById("shelf-index");
  if (!payload) return [];
  try {
    const shelves = JSON.parse(payload.textContent);
    return Array.isArray(shelves) ? shelves : [];
  } catch {
    // A malformed payload is not worth taking the page down for: the
    // shelves are all rendered already, and returning nothing leaves
    // them that way.
    return [];
  }
};

export const showOnly = (key) => () => {
  for (const section of document.querySelectorAll(SECTIONS)) {
    section.hidden = section.dataset.shelf !== key;
  }
};
