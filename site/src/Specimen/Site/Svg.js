// The calendar year of an epoch-millis instant, as the reader's locale
// would name it. Dates are the one thing here the generator can't derive
// from the source it was handed.
export const year = (millis) => String(new Date(millis).getFullYear());
