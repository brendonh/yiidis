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
    foreach($scores as $fragment=>$score) {
      Yii::app()->redis->conn->zadd(
                    $this->redisKey("f", $fragment),
                    $score, $key);
    }
  }

  public function search($phrase, $offset=0, $limit=20) {
    $scores = $this->analyze($phrase);
    $keys = array_keys($scores);
    sort($keys);
    $key = implode("-", $keys);
    $searchKey = $this->redisKey("s", $key);
    $conn = Yii::app()->redis->conn;

    if (!$conn->expire($searchKey, $this->cacheTime)) {
      $args = array();
      $weights = array();
      foreach($scores as $key => $score) {
        $args[] = $this->redisKey("f", $key);
        $weights[] = strlen($key) * $score;
      }
      
      $args = array($searchKey, $args, $weights);
      $command = $conn->createCommand("zunionstore", $args);
      $conn->executeCommand($command);
      $conn->expire($searchKey, $this->cacheTime);
    }
    $results = $conn->zrevrange($searchKey, $offset, $offset + $limit);
    return $results;
  }

  public function analyze($phrase) {
    $phrase = preg_replace('/[^\p{L}\p{N}\p{Z}]/u', '', strtolower($phrase));

    $words = preg_split('/[\p{Z}]/u', $phrase);

    $scores = array();
    foreach($words as $word) {
      if (in_array($word, $this->stopwords)) continue;
      list($a, $b) = double_metaphone(stem($word));
      if (!$a) $a = $word;
      $scores[$a] = isset($scores[$a]) ? $scores[$a]+1 : 1;
      if ($b != $a) isset($scores[$b]) ? $scores[$b]+1 : 1;
    }

    return $scores;
  }

}