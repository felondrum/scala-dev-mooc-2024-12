package ru.otus.module4.homework.services

import io.getquill.context.ZioJdbc.QIO
import ru.otus.module4.homework.dao.entity.{Role, RoleCode, User, UserId}
import ru.otus.module4.homework.dao.repository.UserRepository
import ru.otus.module4.phoneBook.db
import zio.ZLayer
import zio.ZIO
import java.sql.SQLException

trait UserService{
    def listUsers(): QIO[List[User]]
    def listUsersDTO(): QIO[List[UserDTO]]
    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]]
}
class Impl(userRepo: UserRepository) extends UserService {
    val ctx = db.Ctx

    def listUsers(): QIO[List[User]] =
        userRepo.list()


    def listUsersDTO(): QIO[List[UserDTO]] = 
        for {
            users <- userRepo.list()
            dtos <- ZIO.foreach(users) { user =>
                userRepo.userRoles(user.typedId).map(roles => UserDTO(user, roles.toSet))
            }
        } yield dtos


    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO] = 
        ctx.transaction {
            for {
                createdUser <- userRepo.createUser(user)
                _ <- userRepo.insertRoleToUser(roleCode, createdUser.typedId)
                roles <- userRepo.userRoles(createdUser.typedId)
            } yield UserDTO(createdUser, roles.toSet)
        }.mapError {
            case e: SQLException => e
            case e => new SQLException(e)
        }

    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]] = 
        for {
            users <- userRepo.listUsersWithRole(roleCode)
            dtos <- ZIO.foreach(users) { user =>
                userRepo.userRoles(user.typedId).map(roles => UserDTO(user, roles.toSet))
            }
        } yield dtos


}
object UserService{

    val layer: ZLayer[UserRepository, Nothing, UserService] = 
        ZLayer.fromFunction((repo: UserRepository) => new Impl(repo))
}

case class UserDTO(user: User, roles: Set[Role])