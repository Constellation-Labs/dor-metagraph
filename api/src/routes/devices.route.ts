import express from 'express';
const router = express.Router();

import * as devicesController from '../controllers/devicesController'

router.get('/:publicKey', devicesController.getDeviceByPublicKey )

export default router;