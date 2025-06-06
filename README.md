# MigrondiUI

MigrondiUI is a desktop application built with F# and Avalonia, designed to manage database migrations as simple as [migrondi cli].

If you've used [migrondi cli] before, think this as a GUI to manage multiple of those projects!

## Features

- **Database Migrations**: Execute SQL migrations directly within the app.
- **Visualize Existing Projects**: Import your local projects into the app as readonly (you can still execute the migrations).
- **Virtual Project Management**: Create migrondi projects without a physical presence in the system thy're saved in the app's embedded database.
  - **Virtual Project Export**: If you want to "eject" from the GUI app into a normal migrondi cli project, you can export an existing virtual project.
  - **Local Project Import**: You can import an existing project into a virtual project, your physical project won't be touched and a new virtual project will be created with the exact same migrations and configuration.

### Future plans

- Database Selection for virtual projects

  Instead of saving the virtual projects to an embedded database, save them to your favorite database! That way you can edit the same projects from different computers and backup your projects and its migrations.

- AI integration

  Use your own LLMs to help you write SQL or analyze and describe an existing project.

- Styling: we need to make it look neat!
- Docs: Because every project must have good documentation.

## Usage

Run the application:

```pwsh
dotnet run --project src/MigrondiUI
```

## Contributing

Contributions are welcome! Please submit a pull request or open an issue to discuss changes.

[migrondi cli]: https://github.com/AngelMunoz/Migrondi
