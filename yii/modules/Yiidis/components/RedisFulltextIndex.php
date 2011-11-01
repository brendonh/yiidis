<?php

class RedisFulltextIndex {

  public $keyPrefix;
  public $cacheTime = 60;

  public $stopwords = array(
    "the", "a", "an", "of", "at"
  );

  public function __construct($prefix) {
    $this->keyPrefix = $prefix;
  }

  public function redisKey($key, $subKey) {
    return $this->keyPrefix . ':' . $key . ':' . $subKey;
  }

  public function index($key, $phrase) {

    $scores = $this->analyze($phrase);
    foreach($scores as $fragment=>$score) 
      Yii::app()->redis->conn->zadd(
                    $this->redisKey("e", $fragment),
                    $score, $key);

    $scores = $this->analyze($phrase, true);
    foreach($scores as $fragment=>$score)
      Yii::app()->redis->conn->zadd(
                    $this->redisKey("f", $fragment),
                    $score, $key);

  }

  public function search($phrase, $offset=0, $limit=20, $options=array()) {
    $refresh = isset($options['refresh']) && $options['refresh'];
    $and = !(isset($options['and']) && !$options['and']);
    $fuzzy = isset($options['fuzzy']) && $options['fuzzy'];

    $scores = $this->analyze($phrase, $fuzzy);
    $keys = array_keys($scores);
    sort($keys);
    $key = implode("-", $keys);
    $searchKey = $this->redisKey("s", $key);
    $conn = Yii::app()->redis->conn;

    if ($refresh || !$conn->expire($searchKey, $this->cacheTime)) {
      $args = array();
      $weights = array();

      $indexPrefix = $fuzzy ? "f" : "e";
      foreach($scores as $key => $score) {
        $args[] = $this->redisKey($indexPrefix, $key);
        $weights[] = strlen($key) * $score;
      }
      
      $args = array($searchKey, $args, $weights);

      $commandName = $and ? "zinterstore" : "zunionstore";
      $command = $conn->createCommand($commandName, $args);
      $conn->executeCommand($command);
      $conn->expire($searchKey, $this->cacheTime);
    }
    $results = $conn->zrevrange($searchKey, $offset, $offset + $limit);
    return $results;
  }

  public function analyze($phrase, $fuzzy=false) {
    $phrase = preg_replace('/[^\p{L}\p{N}\p{Z}]/u', '', strtolower($phrase));

    $words = preg_split('/[\p{Z}]/u', $phrase);

    $scores = array();
    foreach($words as $word) {
      if (in_array($word, $this->stopwords)) continue;

      if ($fuzzy) $word = stem($word);
      $scores[$word] = isset($scores[$word]) ? $scores[$word]+1 : 1;
    }

    return $scores;
  }

}