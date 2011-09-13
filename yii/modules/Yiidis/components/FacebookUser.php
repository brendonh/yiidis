<?php

class FacebookUser extends RedisModel {

  public static $_keyPrefix = 'user';

  public $name;
  public $perms;

  public function afterConstruct() {
    if (isset(Yii::app()->params->appUserClass)) { 
      $userClass = Yii::app()->params->appUserClass;
      $this->attachBehavior("appUserClass", $userClass);
    }
  }

}