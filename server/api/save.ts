import type { IncomingMessage, ServerResponse } from "http";
import prismaClient from "@prisma/client";
import { useBody, useQuery } from "h3";

const prisma = new prismaClient.PrismaClient();

export default async (req: IncomingMessage, res: ServerResponse) => {
  if (req.method !== "POST") {
    res.statusCode = 404;
    res.end();
    return;
  }

  const body = await useBody(req);
  const boxId = useQuery(req).boxId;

  let box = await prisma.box.findFirst({
    where: {
      id: +boxId,
    },
  });

  if (!box) {
    // res.statusCode = 400;
    // return { error: "Box not found" };

    box = await prisma.box.create({
      data: {
        id: +boxId,
      },
    });
  }

  const pokemon = await prisma.pokemon.create({
    data: {
      name: body.name,
      boxId: box.id,
    },
  });

  return pokemon;
};
