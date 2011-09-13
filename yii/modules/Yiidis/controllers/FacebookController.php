<?php

class FacebookController extends YiidisController {

  public function actions() {
    return array();
  }

  public function actionAfterLogin() {
    Yii::log("Facebook login successful", "info");
    Yii::app()->request->redirect("/");
  }

  public function actionAfterLogout() {
    Yii::log("Facebook logout successful", "info");
    Yii::app()->request->redirect("/");
  }

}