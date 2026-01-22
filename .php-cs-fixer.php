<?php

declare(strict_types=1);

$projectRoot = getcwd();

$finder = PhpCsFixer\Finder::create()
    ->in($projectRoot)
    ->name('*.php');

return (new PhpCsFixer\Config())
    ->setRules([
        '@PSR12' => true,
        'strict_param' => true,
        'no_unused_imports' => true,
        'ordered_imports' => true,
        'array_syntax' => ['syntax' => 'short'],
    ])
    ->setFinder($finder)
    ->setRiskyAllowed(true)
    ->setUsingCache(true);
