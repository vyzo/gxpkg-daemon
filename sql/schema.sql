CREATE TABLE IF NOT EXISTS packages (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  author TEXT,
  name TEXT,
  description TEXT,
  runtime TEXT,
  license TEXT,
  last_update TEXT,
  repo TEXT UNIQUE
);

CREATE TABLE IF NOT EXISTS forks (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  author TEXT,
  name TEXT,
  html_url TEXT UNIQUE,
  package_id INTEGER,
  FOREIGN KEY(package_id) REFERENCES packages(package_id)
);
