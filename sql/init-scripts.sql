CREATE DATABASE okssi_data;
\c okssi_data;

CREATE SCHEMA shop;

CREATE TABLE IF NOT EXISTS shop."Products" (
    sku VARCHAR(255) PRIMARY KEY,
    category VARCHAR(255) NOT NULL,
    name VARCHAR(255) NOT NULL,
    price NUMERIC(10, 2) NOT NULL,
    description TEXT NOT NULL,
    urlimg VARCHAR(255) NOT NULL
);
