<?php

// Slim 4

/*

Install with:
composer require slim/slim:"4.*" slim/psr7 php-di/php-di slim/http phpunit/phpunit

*/

use Slim\Http\Response;
use Slim\Http\ServerRequest;
use Slim\Factory\AppFactory;

require __DIR__ . '/../vendor/autoload.php';

$app = AppFactory::create();

$app->get('/', function ($request, $response, $args) {
    $response->getBody()->write("Hello world!");
    return $response;
});

$app->run();