import { Request, Response } from 'express';
import devices from '../assets/fake_devices_database.json';

const getDeviceByPublicKey = (
  req: Request,
  res: Response
): Response<any, Record<string, any>> => {
  const { publicKey } = req.params;

  const uncompressedPublicKey =
    publicKey.length === 128 ? "04" + publicKey : publicKey;

  const device = devices.find(
    (device) => device.publicKey === uncompressedPublicKey.substring(2)
  );

  if (!device) {
    return res.status(404).json({ error: "Device not found" });
  }

  return res.status(200).json(device);
};

export { getDeviceByPublicKey };
