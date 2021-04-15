create table if not exists software
(
    id             serial primary key,
    name           varchar(64),
    annotation     varchar(64),
    version        int,
    datetime_start varchar(64),
    datetime_end   varchar(64),
    author_id      int references authors (id),
    type_id        int references types (id)
);

create table if not exists authors
(
    id      serial primary key,
    name    varchar(64),
    surname varchar(64)
);

create table if not exists users
(
    id         serial primary key,
    name       varchar(64),
    surname    varchar(64),
    start_date varchar(64)
);

create table if not exists types
(
    id   serial primary key,
    name varchar(100) not null
);

create table if not exists usage_statistics
(
    software_id int references software (id),
    user_id     int references users (id),
    quantity int
);
