<?php

class FacebookUser extends RedisModel {

  public static $_keyPrefix = 'user';

  public $name;

  public function afterConstruct() {
    $userClass = Yii::app()->params->appUserClass;
    $this->attachBehavior("appUserClass", $userClass);
  }
}